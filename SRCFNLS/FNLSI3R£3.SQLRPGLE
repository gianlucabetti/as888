000100990610     H DECEDIT('0,') DATEDIT(*DMY.)
000200150202     H* FNLSI3R *----------------------------------------------------*
000300970410     H*                                                              *
000400150202     H*       - CHIUSURA BOLLE IN PARTENZA CON PRATICA IDD  - NO EDI *
000500050209     H*                                                              *
000600970410     H*--------------------------------------------------------------*
000900150205     FFNBLP01L  uF   E           K DISK
001000970410     F*--------
001100150204     FFNBLT01L  UF   E           K DISK
001101150204     FFNBrv07L  IF   E           K DISK
001102150204     FFiar401l  IF   E           K DISK
001103150206     FFiAR403L  IF   E           K DISK    RENAME(FiAR4000:FiAR4003)
001200970410     F*--------
001300970410     FFNLBL01L  IF   E           K DISK
002700970410     F*--------
002800150219     FTABEL00F  IF   E           K DISK
003100970410     F*--------
003200150219     FAZORG01L  IF   e           k DISK
003300970414     F*--------
003400150202     FFNLSI3D   CF   E             WORKSTN
003401150218     f                                     sfile(lsi1S01 : S01nrr)
003500080918     F*--------
003600150204     FTNTBE02L  IF   E           K DISK
003700970410     D*--------------------------------------------------------------*
003800970410     D*  SCHIERE
003900970410     D*--------------------------------------------------------------*
004000970612     D*  Caricamento capoconto clienti
004100970410     D* Schiera codici tabella 3K: invio date consegna
004200030108     D C3K             S              7  0 DIM(9990)
004300150219     D*CKS             S              7  0 DIM(9990)
004400030108     D CSD             S              2    DIM(9990)
004500970715     D* Tipi bolla: tabella 3A
004600970715     D C3A             S              2    DIM(500)
004700970715     D TPO             S              2    DIM(500)
006100970410     D* Caricamento £1
006200020506     D*UNI             S              3    DIM(29)
006300020506     D £1              S              3  0 DIM(30)
006301150204     D sel             S              1  0 DIM(3)
006400970414     D* ERRORI
006500150507     D ERR             S             78    DIM(33) CTDATA PERRCD(1)
006501150204
006502150204     D kcode           S                   LIKE(tbecod) inz('BSP')
006503150204     D kline           S                   LIKE(tbelin) inz(' ')
006504150204     D ksife           S                   LIKE(tbesif)
006505150204     d wdata           s              8  0
006507150204     D KTRC            S                   LIKE(AR4TRC)
006508150204     D wsel            S              1  0
006509150205     D x               S              5  0
006510150205     D wbsp            S              1    inz
006511150209     D kn30            S                   like(ar4n30)
006512150209     D wksc            S                   like(blpksc)
006513150209     D wlnp            S                   like(blplnp)
006514150217     d s01rcd          s                   like(c01rcd)
006516150217     d $Inzd02         s               n   inz(*on)
006517150217     d $Inzs01         s               n   inz(*on)
006518150217     d $Finerec        s               n   inz(*off)
006519150218     d S01nrr          s              4  0 inz
006520150218     d wopz            s              1a
006521150218     d wtfp            s                   LIKE(BLPTFP)
006522150217
006523150217     D WrkStringaSql   S           4500
006524150217     D                                     VARYING
006525150218     d Datadmy         s               d   datfmt(*dmy)
006526150219     d data_eur        s               D   datfmt(*eur)
006600970410     D*--------------------------------------------------------------*
006700970410     D* DS
006800970410     D*--------------------------------------------------------------*
008100020506     d trul06ds      e ds
008200020506     d  lin                    1     90  0 dim(30)
008201150218     d fnlv55ds      e ds
008900970414     D WLBDA8          DS
009000970414     D  G02DAT                 1      8  0
009100150203     D  G02INV                 9     16  0
009200970414     D  G02ERR                17     17
009300970414     D  G02TGI                18     22  0
009301150204     d WDAT8           DS
009302150204     d  datada                 1      8  0
009303150204     d  dataa                  9     16  0
009304150204     d  ggl                   17     21  0
009800080421      *
009900970410     D KPJBA         E DS
009901150217     d fnblpds       e ds                  extname(fnblp00f) inz
009902150204       // -?Parametri x Controllo profilo utenti?
009903150204     d TIBS34ds      e ds
009904150211     d TIBS69DS      E DS                  INZ
009905150211     d DS_cnaco      E DS                  extname(CNACO00F)
009906150211     d DS_cnind      E DS                  extname(CNIND00F)
009907150211     d DS_cnclp      E DS                  extname(CNCLP00F)
009908150211     d DS_fncls      E DS                  extname(FNCLS00F)
009909150204       // -?Ds di riferimento al file esterno AZUTE00F?
009910150204     d AZUTEds       e ds                  extname(AZUTE00F)
009911150204       // -?Ds per dati organigramma?
009912150204     d dDATIUTE      e ds
010300970616     D* DATI X RICHIAMO PGM INTERR. BOLLE PARTENZE (FNLS05R)
010400970616     D PARA04          DS                  INZ
010500970616     D  P04AAS                 1      4  0
010600970616     D  P04LNP                 5      7  0
010700970616     D  P04NRS                 8      9  0
010800970616     D  P04NSP                10     16  0
010900980204     D  P04F03                17     17
011000970616     D  P04FLG                31     31
011100970616     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
011200970616     D  P04GIA                48     48
011300970616      * PA3CLI indica se il pgm e' richiamato da un cliente (='S')
011400970616     D  P04CLI               143    143
011500970616     D PARA05          DS                  INZ
011600970616     D  P05AAS                 1      4  0
011700970616     D  P05LNP                 5      7  0
011800970616     D  P05NRS                 8      9  0
011900970616     D  P05NSP                10     16  0
012000970616     D  P05F03                17     17
012100970616     D  P05FLG                18     18
012200970616     D  P05SIM                44     46  0
012300970616     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
012400970616     D  P05GIA                48     48
012500080421     D***** TIBS55        E DS                  EXTNAME(TIBS55DS)
012700970410     D DS3K          E DS
012800970715     D DS3A          E DS
013000970410     D DS7O          E DS
013100060811     D OG143         E DS
013101150204     D dbsp          E DS
013102150204     D ds3i          E DS
013103150209     D dsbl4i        E DS
013104150219      * Ds esterna per ricerca cliente da tab. "BSP"
013105150219     d tntbBSPds1    e ds                  inz
013200040715
013300040715     d Fidn12ds      e ds
013400040715     d  skKey                 26    305    dim(20)
013500040715     d  skAnn                306    325    dim(20)
013600040715     d  skDca                326    485    dim(20)
013700040715     d  skFca                486    545  0 dim(20)
013800040715     d  skDch                546    705  0 dim(20)
013900040715     d  skCch                706    745    dim(20)
014000040715
014100970410     D                SDS
014200970410     D  NOMPGM                 1     10
014201150211      /copy gaitrasrc/srcprotopr,tibs69r
014901150211
015000970410     C*--------------------------------------------------------------*
015100970410     C*  C I C L O     P R I N C I P A L E
015200970410     C*--------------------------------------------------------------*
015201150217      /free
015202150217         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
015203150217      /end-free
015204150217
015300970410     C*  Inizializzo varibili videata
015400970410     C                   EXSR      INZ01
015500080421      *
015600970410     C*  Gestione della prima videata
015700970410     C     WFINE         DOWEQ     'N'
015800970611     C     WTPVID        CASEQ     '1'           GESD01
015900970611     C     WTPVID        CASEQ     '2'           GESD02
015901150211     C     WTPVID        CASEQ     'S'           GESsfl
016000970410     C                   END
016100970611     C                   END
016200970410     C*  Fine pgm
016300970410     C                   SETON                                        LR
016400970611     C*--------------------------------------------------------------*
016500970611     C* INZ01: Impostazione della prima videata
016600970611     C*--------------------------------------------------------------*
016700970611     C     INZ01         BEGSR
016800970611     C*
016900970611     C                   SETOFF                                       2840
017000970616     C                   MOVEL     '1'           WTPVID
017200970616     C                   MOVEL     'N'           WFINE
017300970611     C                   MOVEL     NOMPGM        V1CPGM
017400150205     C                   clear                   V1CKSC
017401150204     C                   clear                   V1dKSC
017402150204     C                   clear                   V1clnp
017403150204     C                   clear                   V1cnrs
017404150204     C                   clear                   V1cnsp
017405150220     C                   z-add     2015          V1caas
017406150204     C                   clear                   V1cdpd
017500970611     C*
017600970611     C                   ENDSR
017700970410     C*--------------------------------------------------------------*
017800970611     C* INZ02: Impostazione della prima videata
017900970410     C*--------------------------------------------------------------*
018000970611     C     INZ02         BEGSR
018100970410     C*
018200970410     C                   SETOFF                                       284041
018300970611     C                   SETOFF                                       424344
018301150209     C                   SETOFF                                       45
018500970611     C                   MOVEL     NOMPGM        V2CPGM
018600150204     C                   z-add     blpksc        V2CKSC
018700150204     C                   MOVEL     blprsm        V2DKSC
018800150204     c                   z-add     blplnp        V2clnp
018900150204     C                   z-add     blpnrs        V2Cnrs
019000150204     C                   z-add     blpnsp        v2cnsp
019100150204     C                   z-add     blpaas        V2Caas
019101150204     c     *iso          move      blpdim        data_eur
019102150204     c                   move      data_eur      v2cdim
019103150204     c                   eval      wdata=(blpaas*10000)+blpmgs
019104150204     c     *iso          move      wdata         data_eur
019105150204     c                   move      data_eur      v2cdsp
019106150204     c                   eval      v2crsm=BLPRSM
019107150204     c                   eval      v2cinm=BLPINM
019108150204     c                   eval      v2ccam=BLPCAM
019109150204     c                   eval      v2clom=BLPLOM
019110150204     c                   eval      v2cprm=BLPPRM
019111150204     c                   eval      v2cnzm=BLPnzm
019112150204     C* CERCO LA DESCRIZIONE AGGIUNTIVA PER IL DESTINATARIO
019117150204     C                   MOVEL     'D'           KTRC
019118150204     c                   clear                   ar4not
019119150204     C     Kar41         CHAIN     Fiar401L
019122150204     c                   eval      v2crsd=BLPRSd+ar4not
019123150204     c                   eval      v2cind=BLPINd
019124150204     c                   eval      v2ccad=BLPCAd
019125150204     c                   eval      v2clod=BLPLOd
019126150204     c                   eval      v2cprd=BLPPRd
019127150204     c                   eval      v2cnzd=BLPnzd
020800970410     C*
020900970410     C                   ENDSR
021000970410     C*--------------------------------------------------------------*
021100970410     C* GESD01: Gestione della prima videata
021200970410     C*--------------------------------------------------------------*
021300970410     C     GESD01        BEGSR
021400970410     C*
021500970410     C* Eseguo exfmt della prima videata
021600970410     C                   EXFMT     LSI1D01
021700150203     C                   SETOFF                                       284041
021800150209     C                   SETOFF                                       434445
021801150219     c                   setoff                                       90
021900970410     C* Fine
022000970410     C     *INKC         IFEQ      '1'
022100970414     C                   MOVEL     'S'           WFINE             1
022200150205     C                   leavesr
022300970410     C                   END
022301150204     C* Interrogazione bolle
022302150204     C     *INKG         IFEQ      '1'
022303150204     C     V1CNSP        IFGT      0
022304150204     C                   CLEAR                   PARA05
022305150204     C                   Z-ADD     V1CAAS        P05AAS
022306150204     C                   Z-ADD     V1CLNP        P05LNP
022307150204     C                   Z-ADD     V1CNRS        P05NRS
022308150204     C                   Z-ADD     V1CNSP        P05NSP
022309150204     C                   MOVEL     '1'           P05GIA
022310150204     C                   MOVEL     PARA05        KPJBU
022311150204     C                   CALL      'FNLS05R'
022312150204     C                   PARM                    KPJBA
022313150204     C                   MOVEL     KPJBU         PARA05
022314150204     C     P05F03        IFEQ      *BLANKS
022315150204     C                   Z-ADD     P05AAS        V1CAAS
022316150204     C                   Z-ADD     P05LNP        V1CLNP
022317150204     C                   Z-ADD     P05NRS        V1CNRS
022318150204     C                   Z-ADD     P05NSP        V1CNSP
022319150204     C                   ENDIF
022320150204     C*
022321150204     C                   ELSE
022322150204     C*
022323150204     C                   CLEAR                   PARA04
022324150204     C                   MOVEL     '1'           P04FLG
022325150204     C                   MOVEL     PARA04        KPJBU
022326150204     C                   CALL      'FNLS04R'
022327150204     C                   PARM                    KPJBA
022328150204     C                   MOVEL     KPJBU         PARA04
022329150204     C     P04F03        IFEQ      *BLANKS
022330150204     C                   Z-ADD     P04AAS        V1CAAS
022331150204     C                   Z-ADD     P04LNP        V1CLNP
022332150204     C                   Z-ADD     P04NRS        V1CNRS
022333150204     C                   Z-ADD     P04NSP        V1CNSP
022334150204     C                   ENDIF
022335150204     C                   ENDIF
022336150204     C                   leavesr
022337150204     C                   END
022400970410     C* Controlli
022500970410     C                   EXSR      CTR01
022501150219     c   90
022600150219     Cor 28              leavesr
022700970611     C*
022800970611     C* Se no errori passo a videata successiva
022801150211     c                   if        v1cksc<>*blanks and v1cksc<>*zeros
022802150217     C                   MOVEL     'S'           WTPVID
022803150217     c                   eval      $inzS01=*on
022805150211     c                   else
022901150217     C                   MOVEL     '2'           WTPVID
022902150219     c**                 eval      $inzd02=*on
022903150211     c                   endif
023000970410     C*
023100150203     C                   ENDSR
023200970611     C*--------------------------------------------------------------*
023300970611     C* GESD02: Gestione della seconda videata
023400970611     C*--------------------------------------------------------------*
023500970611     C     GESD02        BEGSR
023501150217     c*
023502150219     c****               if        $inzd02=*on
023503150217     C                   EXSR      INZ02
023504150219     c****               eval      $inzd02=*off
023505150219     c****               endif
023600970611     C*
023700970611     C* Eseguo exfmt della prima videata
023701150219     c                   do        *hival
023800970611     C                   EXFMT     LSI1D02
023900970611     C                   SETOFF                                       284041
024000970611     C                   SETOFF                                       424344
024001150209     C                   SETOFF                                       45
024100970611     C* Fine
024200150219     C*    *INKC         IFEQ      '1'
024300150219     C*                  MOVEL     'S'           WFINE             1
024301150219     c*                  leavesr
024302150219     c*                  endif
025000970611     C* Prima videata
025100970611     C     *INKL         IFEQ      '1'
025101150218     c                   if        wtpvid<>'S'
025200970611     C                   MOVEL     '1'           WTPVID            1
025201150218     c                   endif
025700150204     C                   leavesr
025800970611     C                   END
025900080421      *
029700970611     C* Controlli
029800150204     C***                EXSR      CTR02
029900150204     C***28              GOTO      FINVD2
030000970611     C*
030001150204     c                   if        *inkf
030002150219     c     kblt          chain(n)  fnblp01l
030003150219     c                   exsr      ctr01_uni
030004150219     c                   if        not *in28
030005150204     c                   exsr      sr_ChiuBolla
030006150219     c                   if        not *in28
030007150220     c                   if        wtpvid='2'
030008150204     C                   MOVEL     '1'           WTPVID            1
030009150220     c                   endif
030010150204     C                   leavesr
030011150219     c                   endif
030012150219     c                   endif
030501150204     c                   endif
030502150219
030503150219     c                   enddo
030700970611     C*
030800150204     C                   ENDSR
030801150217     C*--------------------------------------------------------------*
030802150217     C* gessfl:  videata sfl per codice cliente
030803150217     C*--------------------------------------------------------------*
030804150217     C     gessfl        BEGSR
030805150217      /free
030806150217         if $inzs01=*on;
030807150217            exsr inzs01;
030808150217            c01csr=1;
030809150217            c01rcd=1;
030810150217            $inzs01=*off;
030811150217         // Se non trovati record errore in prima videata
030812150217            if s01nrr=0   ;
030813150217               $msg=err(21)  ;
030815150217               *in28=*on  ;
030816150219               *in45=*on  ;
030817150217               wtpvid='1'  ;
030818150217               leavesr   ;
030819150217            endif  ;
030820150217         // Se trovato un solo record emetto direttamente la videata singola
030821150217            if s01nrr=1;
030822151007              chain 1 lsi1s01;
030823151007              chain(n) (vshaas:vshlnp:vshnrs:vshnsp) fnblp01l;
030825150217              wtpvid='2'  ;
030826150217              leavesr;
030827150217            endif;
030828150217         endif;
030829150217       // c01csr   contiene il numero di riga del subfile su cui era posizionato
030830150217       // impostando c01rcd   visualizzo la pagina che vedeva l'utente quando ha
030831150218       // l'ultimo tasto
0308321502172         if c01csr   > 0;
030833150220           c01rcd   = c01csr;
030834150217          else;
030835150217           c01rcd   = s01rcd   ;
0308361502172         endif;
030837150217       // se non so quale pagina visualizzare forzo pagina 1
0308381502172         if c01rcd   < 1;
030839150217           eval c01rcd   = 1;
0308401502172         endif;
030841150217
030842150218       // valorizzo campi del sfl control
030843150218          vc1ksc=%int(v1cksc);
030844150218          vc1kscd=v1dksc;
030845150217
030846150217
030847150217         // Emissione videata
030848150217         write  lsi1z01;
030849150218         exfmt  lsi1C01;
030850150217
030854150217         s01nrr=1  ;
030855150217
030856150218 1       SELECT;
030857150217
030858150217         // - F3=Fine
030859150217 1         WHEN  *inkc;
030865150219               wFine = 'S';
030867150217
030868150217         // - F12=Ritorno
030869150218 1         WHEN  *inkl;
030875150217               wtpvid='1'  ;
030877150217
030887150217
030888150220         // -
030889150220 1       //WHEN  *inkb ;
030891150218         //  exsr seltot;
030896150217
030901150217
030902150217 x1      // Invio
030903150217           OTHER;
030904150217
030907150217               exsr ContrS01 ;
030929150217
030930150217 1       ENDSL;
030931150217      /end-free
030932150217     c                   endsr
047501150203     C*--------------------------------------------------------------*
047502150203     C* CTR01: Controlli prima videata
047503150203     C*--------------------------------------------------------------*
047504150203     C     CTR01         BEGSR
047505150219     c                   clear                   v1dksc
047506150204     c*  verifico la richiesta:
047507150204     c* è possibile entrare per - cod. cliente
047508150204     c*                         - key spedizione
047509150204     c*                         - Numero parcel DPD
047510150204     C                   clear                   sel
047511150204     C                   clear                   wsel
047512150204     c* selezionato cliente
047513150205     c                   if        v1cksc<>*blanks and v1cksc<>*zeros
047514150204     c                   eval      sel(1)=1
047515150204     c                   endif
047516150204     c* selezionata spedizione
047517150204     c                   if        v1cnsp>0
047518150204     c                   eval      sel(2)=1
047519150204     c                   endif
047520150204     c* selezionato n.parcel DPD
047521150204     c                   if        v1cdpd<>*blanks
047522150204     c                   eval      sel(3)=1
047523150204     c                   endif
047524150204     c                   xfoot     sel           wsel
047525150204     c                   if        wsel>1
047526150204     C                   SETON                                        28
047527150205     C                   MOVEL     ERR(30)       $MSG
047528150204     c                   leavesr
047529150204     c                   endif
047530150507     c                   if        wsel=0
047531150507     C                   SETON                                        28
047532150507     C                   MOVEL     ERR(33)       $MSG
047533150507     c                   leavesr
047534150507     c                   endif
047535150204     c                   select
047536150204     c* selezione per codice cliente
047537150204     c                   when      sel(1)=1
047538150205     c                   exsr      ctr01_ksc
047539150204     c* selezione per key spedizione
047540150204     c                   when      sel(2)=1
047541150204     c                   exsr      ctr01_nsp
047542150204     c* selezione per numero parcel dpd
047543150204     c                   when      sel(3)=1
047544150204     c                   exsr      ctr01_dpd
047545150204     c                   endsl
047546150204     c                   endsr
047547150204     c
047548150204     C*--------------------------------------------------------------*
047549150204     C* CTR01_nsp - Controlli prima videata - Accesso per key spedizione
047550150204     C*--------------------------------------------------------------*
047551150204     C     CTR01_nsp     BEGSR
047900130822     C*
048000970410     C* Controllo se immesso anno
048100150203     C     V1CAAS        IFEQ      0
048200970410     C                   SETON                                        2840
048300970611     C                   MOVEL     ERR(5)        $MSG
048301150203     c                   leavesr
048400970410     C                   END
048600970410     C* Se anno digitato di 2 --> 4 cifre
048700150203     C                   MOVEL     V1CAAS        WSECOL            2 0
048800970410     C     WSECOL        IFEQ      0
048900150203     C     V1CAAS        IFGT      60
049000150203     C                   ADD       1900          V1CAAS
049100970410     C                   ELSE
049200150203     C                   ADD       2000          V1CAAS
049300970410     C                   END
049400970410     C                   END
049500970410     C* Controllo se immessa linea di partenza
049600150203     C     V1CLNP        IFEQ      0
049700970414     C                   SETON                                        2841
049800970611     C                   MOVEL     ERR(6)        $MSG
049801150203     c                   leavesr
049900970410     C                   END
050100970410     C* Controllo esistenza lnp
050200150203     C                   Z-ADD     V1CLNP        ORGFIL
050300150219     C     ORGFIL        CHAIN     AZORG01L
050400150219     C                   if        not %found(azorg01l)
050500970410     C                   SETON                                        2841
050600970611     C                   MOVEL     ERR(7)        $MSG
050601150203     c                   leavesr
050700970414     C                   END
050701150204     c                   movel     orgde3        og143
050900970414     C* Controllo se lnp in £1
051000150203     C     V1CLNP        LOOKUP    £1                                     32
051100970414     C     *IN32         IFEQ      '0'
051200970414     C                   SETON                                        2841
051300970611     C                   MOVEL     ERR(8)        $MSG
051301150203     c                   leavesr
051400970414     C                   END
051600970414     C* Controllo se immesso numero di spedizione
051700150203     C     V1CNSP        IFEQ      0
051800970414     C                   SETON                                        2843
051900970611     C                   MOVEL     ERR(9)        $MSG
051901150203     c                   leavesr
052000970414     C                   END
052200970414     C* Controllo se esiste spedizione
052300150203     C                   Z-ADD     V1CAAS        KAAS
052400150203     C                   Z-ADD     V1CLNP        KLNP
052500150203     C                   Z-ADD     V1CNRS        KNRS
052600150203     C                   Z-ADD     V1CNSP        KNSP
052700150219     C     KBLP          CHAIN(N)  FNBLP01L
052800150219     C                   if        not %found(fnblp01l)
052900970414     C                   SETON                                        2843
053000970611     C                   MOVEL     ERR(10)       $MSG
053001150203     c                   leavesr
053800970616     C                   END
053801150209     c                   exsr      ctr01_uni
053802150209     c                   if        *in28
053803150209     c                   seton                                        43
053804150209     c                   endif
066257150204     c
066300040715      **
068400970414     C*
068500150203     C                   ENDSR
068501150205     C*--------------------------------------------------------------*
068502150205     C*
068503150205     C*--------------------------------------------------------------*
068504150205     C     ctr01_ksc     BEGSR
068505150219     c                   if        %scan( '?' : V1Cksc ) > *zero
068506150219     c                   clear                   V1Cksc
068507150219     c                   clear                   tntbBSPds1
068508150303     c                   eval      ibsptfp=duttfp
068509150219     c                   eval      KPJBU = tntbBSPds1
068510150219     c                   call      'TNTBBSPR1'
068511150219     c                   parm                    KPJBA
068512150219     c                   eval      tntbBSPds1 = KPJBU
068513150219     c                   if        oBSPfxx <= *zero  and
068514150219     c                             oBSPerr  = *off
068515150219     c                   eval      V1Cksc = %editc( oBSPcli : 'X' )
068516150219     c                   eval      V1Dksc = oBSPdes
068517150219     c                   endif
068518150219     c                   eval      *in45 = *on
068519150219     c                   eval      *in90 = *on
068520150219     c                   leavesr
068521150219     c                   endif
068522150209     c* Il codice o la filiale cliente deve esistere in BSP
068523150211     c                   move      v1cksc        wksc
068524150209     c                   movel     v1cksc        wlnp
068525150209     c                   exsr      cer_bsp
068526150209     c                   if        wbsp=' '
068527150209     c                   seton                                        2845
068528150209     C                   MOVEL     ERR(32)       $MSG
068529150209     c                   leavesr
068530150209     c                   endif
068531150209     c                   clear                   og143
068532150219     C     wlnp          CHAIN     AZORG01L
068533150209     c                   movel     orgde3        og143
068534150209     c* il cliente non deve essere EDI
068535150209     C                   Z-ADD     1             XX                5 0
068536150211     c     wksc          lookup    c3k(xx)                                32
068537150209     c                   if        (*in32 and csd(xx)='EW' ) or
068538150209     c                             (not *in32 and §ogntw='DPD')
068539150209     c                   seton                                        2845
068540150209     C                   MOVEL     ERR(31)       $MSG
068541150209     c                   endif
068542150209     c* decodifico il cliente
068543150211     c                   eval      i69kac=wksc
068544150211     c                   call      'TIBS69R'
068545150211     c                   parm                    tibs69ds
068546150211     c                   parm                    ds_cnaco
068547150211     c                   parm                    ds_cnind
068548150211     c                   parm                    ds_cnclp
068549150211     c                   parm                    ds_fncls
068550150211     c                   eval      v1dksc=acorag
068551150205     c                   endsr
068552150205     C*--------------------------------------------------------------*
068553150205     C*
068554150205     C*--------------------------------------------------------------*
068555150205     C     ctr01_dpd     BEGSR
068556150205     c*
068557150209     c                   clear                   wTrovS            1
068558150209     c                   clear                   wDpdOk            1
068559150206     c                   movel(p)  v1cdpd        kn30
068560150206     c                   movel     'I'           ktrc
068561150206     c     kar403        setll     fiar403l
068562150206     c     ktrc          reade     fiar403l
068563150209    1c                   dow       not %eof(fiar403l)
068564150209     c                   movel     ar4n30        dsbl4i
068565150209    2c                   if        §b4ipn=v1cdpd
068566150209     c                   z-add     ar4aas        kaas
068567150209     c                   z-add     ar4lnp        klnp
068568150209     c                   z-add     ar4nrs        knrs
068569150209     c                   z-add     ar4nsp        knsp
068570150209     c     kblp          chain(n)  fnblp01l
068571150209    3c                   if        %found(fnblp01l)
068572150209     c                   eval      wTrovS='1'
068573150209    3c                   endif
068574150209     C* Controllo se lnp in £1
068575150209    3C                   if        %lookup(blplnp:£1)>0
068576150209     c                   exsr      ctr01_uni
068577150209    4c                   if        not *in28
068578150209     c                   eval      wDpdOk='1'
068579150209     c                   leavesr
068580150209    4c                   endif
068581150209   x3c                   else
068582150209     c                   movel     err(8)        $msg
068583150209    3c                   endif
068584150209     c                   else
068585150209     c                   if        §b4ipn>v1cdpd
068586150209     c                   leave
068587150209     c                   endif
068588150209    2c                   endif
068589150206     c     ktrc          reade     fiar403l
068590150209    1c                   enddo
068591150209     c* se non ho trovato la spedizione restituisco errore
068592150209    1c                   if        wTrovS=*blanks
068593150209     c                   seton                                        2844
068594150209     C                   MOVEL     ERR(10)       $MSG
068595150209     c                   leavesr
068596150209    1c                   endif
068597150209    1c                   if        wDpdOk=*blanks
068598150209     c                   seton                                        2844
068599150209     c                   leavesr
068600150209    1c                   endif
068601150205     c                   endsr
068602150209     C*--------------------------------------------------------------*
068603150209     C*    Controlli unici per tutti i tipi di entrata
068604150209     C*--------------------------------------------------------------*
068605150209     C     ctr01_uni     BEGSR
068606150209     C* Controllo se bolla figlia
068607150219     C                   Z-ADD     blpAAS        KAAS
068608150209     C                   Z-ADD     blpLNP        KLNP
068609150219     C                   Z-ADD     blpNRS        KNRS
068610150209     C                   Z-ADD     blpnsp        KNSP
068611150219     C     KBLP          CHAIN     FNLBL01L
068612150219     C                   if        %found(fnlbl01l)
068613150209     C                   SETON                                        28
068614150209     C                   MOVEL     ERR(12)       $MSG
068615150209     c                   leavesr
068616150209     C                   END
068617150209     C* Controllo che non sia stata data per consegnata
068618150209     C     BLPDCM        IFNE      0
068619150209     C     BLPCCA        ANDNE     '6'
068620150209     C     BLPDCM        ORNE      0
068621150209     C     BLPCCA        ANDNE     '2'
068622150209     C                   SETON                                        28
068623150209     C                   MOVEL     ERR(22)       $MSG
068624150209     c                   leavesr
068625150209     C                   END
068626150209     C* Controllo che abbia il dettaglio segnacolli
068627150209     C***                Z-ADD     V1CAAS        KAAS
068628150209     C***                Z-ADD     V1CLNP        KLNP
068629150209     C***                Z-ADD     V1CNRS        KNRS
068630150209     C***                Z-ADD     V1CNSP        KNSP
068631150219     C     KBLT          CHAIN(n)  FNBLT01L
068632150219     C                   IF        not %found(fnblt01l)
068633150209     C                   SETON                                        28
068634150209     C                   MOVEL     ERR(16)       $MSG
068635150209     c                   leavesr
068636150209     C                   END
068637150209     C* Se bolla in assegnato no IDD
068638150209     C                   Z-ADD     1             X
068639150209     C     BLPCBO        LOOKUP    C3A(X)                                 32
068640150209     C                   MOVEL     TPO(X)        WTPO              1
068641150209     C     *IN32         IFEQ      '1'
068642150209     C     WTPO          ANDEQ     'A'
068643150209     C                   SETON                                        28
068644150209     C                   MOVEL     ERR(20)       $MSG
068645150209     c                   leavesr
068646150209     C                   END
068647150209     C* Controllo che non sia stata data per partita
068648150209     C     BLPDT1        IFNE      0
068649150209     C                   SETON                                        28
068650150209     C                   MOVEL     ERR(13)       $MSG
068651150209     c                   leavesr
068652150209     C                   END
068653150209     C* Controllo che non sia stata trasmessa in sede
068654150209     C     BLPDT2        IFNE      0
068655150209     C                   SETON                                        28
068656150209     C                   MOVEL     ERR(14)       $MSG
068657150209     c                   leavesr
068658150209     C                   END
068659150209     C* Controllo che non sia stata trasmessa al cliente
068660150209     C     BLPDT3        IFNE      0
068661150209     C                   SETON                                        28
068662150209     C                   MOVEL     ERR(15)       $MSG
068663150209     c                   leavesr
068664150209     C                   END
068665150209      * Controllo C.A.
068666150209      *
068667150209     c                   clear                   fidn12ds
068668150209     c                   eval      i12aas = blpaAS
068669150209     c                   eval      i12lnp = blpLNP
068670150209     c                   eval      i12nrs = blpNRS
068671150209     c                   eval      i12nsp = blpNSP
068672150209     c                   eval      i12fel = 'E'
068673150209     c                   eval      i12fan = 'E'
068674150209     c                   eval      i12fch = 'E'
068675150209     c                   call      'FIDN12R'
068676150209     c                   parm                    fidn12ds
068677150209      **
068678150209      * se c'è almeno una C.A. aperta
068679150209     c     o12nca        ifgt      0
068680150209     C                   SETON                                        28
068681150209     C                   MOVEL     ERR(24)       $MSG
068682150209     c                   leavesr
068683150209     c                   end
068684150209     c* controllo che la spedizione non abbia spunte
068685150209     c                   clear                   wsispu            1
068686150209     c     kblt          setll     fnblt01l
068687150209     c     kblt          reade(n)  fnblt01l                               99
068688150209    2c                   dow       not *in99
068689150209     c
068690150219     c* Cerco una qualsiasi spunta
068691150209     c     kbrv          chain     fnbrv07l
068692150209    3c                   if        %found(fnbrv07l)
068693150209     c                   eval      wsispu='S'
068694150209     c                   seton                                        99
068695150209   x3c                   else
068696150209     c     kblt          reade(N)  fnblt01l                               99
068697150209    3c                   endif
068698150209    2c                   enddo
068699150209     c*
068700150209    2c                   if        wsispu='S'
068701150209     c                   seton                                        28
068702150209     C                   MOVEL     ERR(29)       $MSG
068703150209     c                   leavesr
068704150209     c                   endif
068705150209     c* il cliente o la lnp della bolla deve essere in tabella BSP con
068706150209     c* la sospensione da spunte
068707150209     c                   z-add     blpksc        wksc
068708150209     c                   z-add     blplnp        wlnp
068709150209     c                   exsr      cer_bsp
068710150209     c                   if        wbsp=' '
068711150209     c                   seton                                        28
068712150209     C                   MOVEL     ERR(27)       $MSG
068713150209     c                   leavesr
068714150209     c                   endif
068715150209     c* il cliente non deve essere EDI
068716150209     C                   Z-ADD     1             XX                5 0
068717150209     c     blpksc        lookup    c3k(xx)                                32
068718150209     c                   if        (*in32 and csd(xx)='EW' ) or
068719150209     c                             (not *in32 and §ogntw='DPD')
068720150209     c                   seton                                        28
068721150209     C                   MOVEL     ERR(31)       $MSG
068722150209     c                   endif
068723150209     c
068724150209     c* per poter chiudere la spedizione come merce mai affidata
068725150209     c* devono essere trascorsi i giorni max di sospensione previsti
068726150209     c* da tabella
068727150209     c                   clear                   wdat8
068728150209     c                   z-add     blpdim        datada
068729150209     c                   eval      dataa=(blpaas*10000)+blpmgs
068730150209     c                   call      'XSRLAV8'
068731150209     c                   parm                    wdat8
068732150209     c                   if        (ggl-1) <= §3iggc
068733150209     c                   seton                                        28
068734150209     C                   MOVEL     ERR(28)       $MSG
068735150209     c                   leavesr
068736150209     c                   endif
068737150209     c
068738150209     c                   endsr
068739150217      /free
068740150211       //--------------------------------------------------------------
068741150211       BEGSR InzS01;
068742150211
068743150211       // Pulizia subfile
068744150216         *in30  = *on;
068745150216         *in31  = *on;
068746150211         write  lsi1c01;
068747150216         *in31  = *off;
068748150216         *in30  = *off;
068749150216         *in32  = *off;
068757150211
068759150217         exec sql close BLPcsr;
068760150217       // Richiamo routine di preparazione stringa SQL
068761150217             exsr sr_prepsql;
068762150217
068763150211         S01nrr=0 ;
068764150211         clear $msg;
068765150216         *in28  = *off;
068766150216         *in90  = *off;
068767150728         *in19  = *off;
068768150217       // Elaboro i records estratti
068769150217           $finerec=*off;
068770150217           exec sql prepare STRINGASQL from :WrkStringaSql;
068771150217           exec sql declare BLPCSR cursor for StringaSql;
068772150217           exec sql open BLPcsr;
068773150217           dow $finerec=*off;
068774150218              exec sql Fetch BLPcsr into :fnblpds;
068775150217              if sqlcod=100 or sqlcod<0;
068776150217                 $finerec = *on;
068777150217                 leave;
068778150217              endif;
068779150218              if %lookup(blplnp:£1)>0;
068780150928                 *in28=*off ;
068781150218                 exsr ctr01_uni;
068782150218                 if not *in28;
068783150217           // Carico record nel sfl
068784150218                     exsr carsf01;
068785150218                 endif;
068786150218              endif;
068787150217           enddo;
068788150211
068789150217          s01rcd    = s01nrr  ;
068790150219          *in28=*off ;
068791150211
068792150211         ENDSR  ;
068793150217       //--------------------------------------------------------------
068794150217       //?Preparazione stringa SQL
068795150217       //--------------------------------------------------------------
068796150217       BEGSR sr_prepsql;
068797150217       WrkStringaSql='select * from fnblp00f' +
068798150218       ' where (blpksc=' + v1cksc + ' or '      +
068799150218       ' blpccm=' + v1cksc + ') and blptfp=' + %editc(wtfp:'X') +
068800150218       ' and blpnfv=0' +
068801150217       ' order by  blptfp, blpnfv, blpdim';
068943150217       endsr;
068944150217       //--------------------------------------------------------------
068945150217       //?eseguo caricamento SFL
068946150217       //--------------------------------------------------------------
068947150217          BEGSR   carSF01    ;
068948150218          clear vs1opz  ;
068949150217
068950150217          exsr CarRECORD  ;
068955150218          vshaas=blpaas;
068956150218          vshlnp=blplnp;
068957150218          vshnrs=blpNRs;
068958150218          vshnsp=blpNsp;
068962150217          s01nrr=s01nrr+1   ;
068963150217          write  lsi1S01;
068964150217
068965150217          ENDSR         ;
068966150217       //--------------------------------------------------------------
068967150217       //?Caricamento record SFL
068968150217       //--------------------------------------------------------------
068969150217       BEGSR CarRecord;
068970150218       // data Immissione bolla
068971150218             clear vs1dim;
068973150218             datadmy=%date(blpdim:*iso);
068974150218             vs1dim=%dec(datadmy);
068975150218       // Data Spedizione
068976150218             clear vs1dsp;
068977150219             eval datadmy=%date((blpaas*10000+blpmgs):*iso);
068978150218             vs1dsp=%dec(datadmy);
068979150219       // linea di arrivo
068980150219             vs1lna=blplna;
068981150219             chain blplna azorg01l ;
068982150219             if orgde5<>*blanks;
068983150219                 vs1lnad=%subst(orgde5:1:10);
068984150219             else;
068985150219                 vs1lnad=%subst(orgdes:1:10);
068986150219             endif;
068987150218       // Destinatario
068988150218             vs1rsd=blprsd;
069042150217       ENDSR         ;
069043150218       //--------------------------------------------------------------
069044150218       //?controlli SFL
069045150218       //--------------------------------------------------------------
069046150218       BEGSR ContrS01;
069047150218
069048150218       s01nrr=1  ;
069049150218       readc lsi1s01    ;
069050150218
069051150218 0     dow not %eof                      ;
069052150218
069053150218       // Verifica l'opzione
069054150218          if vs1opz<>*blanks    ;
069055150218            *in19=*off;
069056150218            wopz='1';
069058150218          select;
069059150218          when vs1opz='1'                     ;
069060150219            chain(n) (vshaas:vshlnp:vshnrs:vshnsp) fnblp01l ;
069066150218            exsr gesd02;
069069150218            chain(n) (vshaas:vshlnp:vshnrs:vshnsp) fnblp01l ;
069071150218            if blpdcm>0;
069072150218               eval *in19=*on;
069075150218            endif;
069076150218          when vs1opz='5';
069077150218               exsr int_bolla;
069084150218          endsl;
069085150219               clear vs1opz;
069086150218               update  lsi1s01  ;
069087150218          endif;
069088150218
069089150218       readc    lsi1s01    ;
069090150218 0     enddo         ;
069091150218
069092150218       ENDSR  ;
069116150218       //--------------------------------------------------------------
069117150218       //?Aggiornamento sfl
069118150218       //--------------------------------------------------------------
069119150218       BEGSR AGGIOSFL  ;
069120150218
069121150218       //Sflnxtchg=*on  ;
069122150218
069123150218       update  lsi1s01  ;
069124150218
069125150218       ENDSR   ;
069126150218       //--------------------------------------------------------------
069127150218       //?Cerca cliente in tabella BSP
069128150218       //--------------------------------------------------------------
069129150218       BEGSR cer_bsp;
069130150218       clear wbsp;
069131150218       ksife=knsif;
069132150218       setll (kcode: kline: ksife) tntbe02l;
069133150218       if not %equal(tntbe02l);
069134150218          clear ksife;
069135150218          setll (kcode: kline: ksife) tntbe02l;
069136150218       endif;
069137150218       if %equal(tntbe02l);
069138150218          reade (kcode: kline: ksife) tntbe02l;
069139150218          dow not %eof(tntbe02l) and wbsp=' ';
069140150218             if tbeatb=' ' and (tbeke2=%editc(wksc:'X') or
069141150218                                tbeke2=%editc(wlnp:'X'));
069142150218                wbsp='S';
069143150218                dbsp=tbeuni;
069144150218             endif;
069145150218             reade (kcode: kline: ksife) tntbe02l;
069146150218          enddo;
069147150218       endif;
069148150218       endsr;
069150150217      /end-free
069151150218     C*--------------------------------------------------------------*
069152150218     C* interroga bolla in partenza
069153150218     C*--------------------------------------------------------------*
069154150218     C     int_bolla     BEGSR
069155150218     C                   CLEAR                   PARA05
069156150218     C                   Z-ADD     VshAAS        P05AAS
069157150218     C                   Z-ADD     VshLNP        P05LNP
069158150218     C                   Z-ADD     VshNRS        P05NRS
069159150218     C                   Z-ADD     VshNSP        P05NSP
069160150218     C                   MOVEL     '1'           P05GIA
069161150218     C                   MOVEL     PARA05        KPJBU
069162150218     C                   CALL      'FNLS05R'
069163150218     C                   PARM                    KPJBA
069164150218     c                   endsr
078601150204     C*--------------------------------------------------------------*
078602150204     C* Chiusura bolla per merce mai affidata
078603150204     C*--------------------------------------------------------------*
078604150204     C     sr_ChiuBolla  BEGSR
078605150219     C     KBLT          CHAIN(e)  FNBLP01L
078606150219     c                   if        %error
078607150219     c                   eval      $msg=err(11)
078608150219     c                   seton                                        28
078609150219     c                   leavesr
078610150219     c                   endif
078611150204     C     KBLT          CHAIN     FNBLT01L                           32
078612150204     C     *IN32         DOWEQ     '0'
078613150204     C                   Z-ADD     OGGI          BLTDFV
078614150303     C                   Z-ADD     99998         BLTNFV
078615150204     C                   Z-ADD     83            BLTNPP
078616150204     C                   Z-ADD     OGGI          BLTDCM
078617150204     C                   Z-ADD     OGGI          BLTDTR
078618150204     C                   MOVEL     'N'           BLTFTR
078619150204     C                   MOVEL     '7'           BLTFL1
078620150204     C                   UPDATE    FNBLT000
078621150204     C* Lettura successiva
078622150204     C     KBLT          READE     FNBLT01L                               32
078623150204     C                   ENDdo
078624150204
078625150204     C                   Z-ADD     0             BLPDCE
078626150204     C                   Z-ADD     0             BLPNPI
078627150204     C                   Z-ADD     0             BLPNCI
078628150204     C                   Z-ADD     0             BLPNRC
078629150204     C                   Z-ADD     OGGI          BLPDBR
078630150204     C                   Z-ADD     OGGI          BLPDPC
078631150204     C                   Z-ADD     OGGI          BLPDUC
078632150204     C                   Z-ADD     OGGI          BLPDDC
078633150204     C                   Z-ADD     OGGI          BLPDCM
078634150204     C                   TIME                    WTIME             6 0
078635150204     C                   MOVEL     WTIME         BLPHMC
078636150204     C                   Z-ADD     99999         BLPNFV
078637150204     C                   Z-ADD     9999999       BLPNDC
078638150204     C                   Z-ADD     OGGI          BLPDT1
078639150204     C                   Z-ADD     OGGI          BLPDT2
078640150204     C                   MOVEL     'N'           BLPFT1
078641150204     C                   MOVEL     'N'           BLPFT2
078642150204     C* Per idd metto codice consegna anomala sulla bolla
078643150204     C                   MOVEL     '7'           BLPCCA
078650150204     C* devo inviare date consegna cliente
078651150303     C                   clear                   BLPDT3
078652150303     C                   clear                   BLPFT3
078653150204     C* Update bolla
078654150204     C                   UPDATE    FNBLP000
078655150204     c                   endsr
078709970410     C*--------------------------------------------------------------*
078800970410     C* *INZSR: Operazioni iniziali
078900970410     C*--------------------------------------------------------------*
079000970410     C     *INZSR        BEGSR
079100970410     C*
079200970410     C     *ENTRY        PLIST
079300970410     C                   PARM                    KPJBA
079301150204
079302150204     c     *dtaara       define    §azute        AZUTEds
079303150204     c     *dtaara       define    §datiute      dDATIUTE
079304150204     c                   in(E)     *dtaara
079305150204     c                   if        %ERROR or RSUT = *blanks
079306150204     c                   clear                   Tibs34Ds
079307150204     c                   call      'TIBS34R'
079308150204     c                   parm                    Tibs34Ds
079309150204     c                   in        *dtaara
079310150204     c                   endif
079311150204      *
079312150204     c                   z-add     1             CodUt             1 0
080000030924      *
080700030924      *
081800970410      *  Definizione chiavi
081900970410     C     KBLP          KLIST
082000970410     C                   KFLD                    KAAS
082100970410     C                   KFLD                    KLNP
082200970410     C                   KFLD                    KNRS
082300970410     C                   KFLD                    KNSP
082400970410     C     KBLT          KLIST
082500150204     C                   KFLD                    blpaas
082600150204     C                   KFLD                    blplnp
082700150204     C                   KFLD                    blpnrs
082800150204     C                   KFLD                    blpnsp
082801150204     C     Kar41         KLIST
082802150204     C                   KFLD                    blpaas
082803150204     C                   KFLD                    blplnp
082804150204     C                   KFLD                    blpnrs
082805150204     C                   KFLD                    blpnsp
082806150204     C                   KFLD                    ktrc
082807150209     C     Kar403        KLIST
082809150209     C                   KFLD                    ktrc
082810150209     C                   KFLD                    kn30
082811150204     C     KBrv          KLIST
082812150204     C                   KFLD                    bltfls
082813150204     C                   KFLD                    bltlna
082814150204     C                   KFLD                    bltNRS
082815150204     C                   KFLD                    bltnsc
083900970410     C     KTAB1         KLIST
084000970410     C                   KFLD                    KKUT
084100970410     C                   KFLD                    KCOD
084200970410     C     KTAB2         KLIST
084300970410     C                   KFLD                    KKUT
084400970410     C                   KFLD                    KCOD
084500970410     C                   KFLD                    KKEY
085900970410      *  Definizione variabili
086200970410     C     *LIKE         DEFINE    BLPAAS        KAAS
086300970410     C     *LIKE         DEFINE    BLPLNP        KLNP
086400970410     C     *LIKE         DEFINE    BLPNRS        KNRS
086500970410     C     *LIKE         DEFINE    BLPNSP        KNSP
087300970410     C     *LIKE         DEFINE    TBLKUT        KKUT
087400970410     C     *LIKE         DEFINE    TBLCOD        KCOD
087500970410     C     *LIKE         DEFINE    TBLKEY        KKEY
087900970410      *
088000970410      *  Reperisco la data del giorno
088100970410     C                   TIME                    WHHDAT           14 0
088200970414     C                   MOVE      WHHDAT        G02DAT
088300970414     C                   MOVEL     *BLANK        G02ERR
088400970414     C                   CALL      'XSRDA8'
088500970414     C                   PARM                    WLBDA8
088600970414     C                   Z-ADD     G02INV        OGGI              8 0
094000970611     C*
094100970410     C* Eseguo caricamento tabella 3K
094300970410     C                   Z-ADD     0             X
094400970410     C                   Z-ADD     1             KKUT
094500970410     C                   MOVEL     '3K'          KCOD
094600970410     C     KTAB1         CHAIN     TABEL                              30
094700970410     C     *IN30         DOWEQ     '0'
094800030213     C     X             ANDLT     9990
094900970410     C     TBLFLG        IFEQ      *BLANKS
095000970410     C                   ADD       1             X
095100970617     C                   MOVEL     TBLUNI        DS3K
095200970617     C                   MOVEL     TBLKEY        C3K(X)
095300150219     C****               MOVEL     §3KCKS        CKS(X)
095400010725     C                   MOVEL     §3KCSD        CSD(X)
095500970410     C                   END
095600970410     C     KTAB1         READE     TABEL                                  30
095700970410     C                   END
095800970715     C* Eseguo caricamento tabella 3A
095900970721     C                   Z-ADD     0             X
096000970715     C                   MOVEL     '3A'          KCOD
096100970715     C     KTAB1         CHAIN     TABEL                              30
096200970715     C     *IN30         DOWEQ     '0'
096300970721     C     X             ANDLT     500
096400970715     C     TBLFLG        IFEQ      *BLANKS
096500970715     C                   ADD       1             X
096600970715     C                   MOVEL     TBLUNI        DS3A
096700970715     C                   MOVEL     TBLKEY        C3A(X)
096800970715     C                   MOVEL     §3ATB1        TPO(X)
096900970715     C                   END
097000970715     C     KTAB1         READE     TABEL                                  30
097100970715     C                   END
098901150204     C***
098902150204     C                   MOVEL     '3I'          Kcod
098903150204     C                   MOVEL     '1       '    kKEY
098904150204     c                   clear                   ds3i
098905150204     C     KTAB2         CHAIN     TABEL00F
098906150205     c                   if        %found(tabel00f)
098907150204     C                   MOVEL     TBLUNI        DS3I
098908150204     c                   endif
098909150204     C***
099000970410     C* Eseguo caricamento £1
099100020506     c                   clear                   trul06ds
099200020506     c                   eval      d06cod = '£1'
099300150205     c                   movel     dutpou        d06key
099400020506     c                   movel     trul06ds      kpjbu
099500020506     c                   call      'TRUL06R'
099600020506     c                   parm                    kpjba
099700020506     c                   movel     kpjbu         trul06ds
099800020506     c                   movea     lin           £1
099801150218     c* terminal di partenza del p.o. utente
099802150218     c                   clear                   fnlv55ds
099803150218     C                   MOVEL     'P'           D55TPT
099804150218     C                   MOVEL     dutpou        D55LIN
099805150218     C                   z-add     oggi          D55drf
099806150218     C                   CALL      'FNLV55R'
099807150218     C                   PARM                    fnlv55ds
099808150218     c                   z-add     d55tfp        wtfp
099900020912      *
100400970410     C*
100500970410     C                   ENDSR
100600080930      *------------------------------------------------------------------------*
100700080930      * Routine di controllo x eventuali Errori
100800080930      *------------------------------------------------------------------------*
100900080930     C     *PSSR         BEGSR
101800080930      *
101900080930     C                   ENDSR     '*CANCL'
102000080930     C*------------------------------------------------------------------------*
102100971008**
102200150220                                                                              01
102300150220                                                                              02
102400150220                                                                              03
102500150220                                                                              04
102600970701Immettere anno della spedizione                                               05
102700970701Immettere linea di partenza della spedizione                                  06
102800970701Linea di partenza inesistente                                                 07
102900150209Spedizione non in gestione alla filiale                                       08
103000970701Immettere numero della spedizione                                             09
103100970701Spedizione inesistente                                                        10
103101150219Richiesta non elaborabile: bolla allocata da altro utente. Attendi e riprova  11
103300980206Bolla figlia: impossibile gestirla                                            12
103400970701Spedizione già trasmessa alla filiale di arrivo                               13
103500970701Spedizione già trasmessa alla sede                                            14
103600970701Spedizione già trasmessa al cliente                                           15
103700980202Spedizione senza colli: impossibile eseguire chiusura                         16
103800980202Spedizione non appartenente al cliente selezionato                            17
103900150220                                                                              18
104000150220                                                                              19
104100980202Bolla in assegnato: chiusura definitiva possibile solo in arrivo              20
104101150217Nessun dato da visualizzare per la selezione richiesta
104300980204Spedizione già consegnata                                                     22
104400150220                                                                              23
104500150220Esistono C.A. aperte per la spedizione                                        24
104600150220                                                                              25
104700150220                                                                              26
104800150205Sped. non gestibile:non prevista dalla Sospensione Bolle x colli non spuntati 27
104900150205Sped. non gestibile:non sono ancora trascorsi i giorni max previsti x sospen. 28
105000150205Spedizione non gestibile:presenti spunte                                      29
105100150204Effettuare una sola selezione                                                 30
105200150209Richiesta non elaborabile:cliente E.D.I.                                      31
105300150303Cliente non previsto dalla sospensione Bolle x colli non spuntati             32
105400150507Effettuare almeno una selezione                                               33
