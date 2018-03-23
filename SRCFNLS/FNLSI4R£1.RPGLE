000100990610     H DECEDIT('0,') DATEDIT(*DMY.)
000200150202     H* FNLSI3R *----------------------------------------------------*
000300970410     H*                                                              *
000400151001     H*       - Ripristino bolle chiuse per merce mai affidata       *
000500050209     H*                                                              *
000600970410     H*--------------------------------------------------------------*
000900150205     FFNBLP01L  uF   E           K DISK
001000970410     F*--------
001100150204     FFNBLT01L  UF   E           K DISK
001101150204     FFNBrv07L  IF   E           K DISK
001102150204     FFiar401l  IF   E           K DISK
001103151001     F*--------
003200150219     FAZORG01L  IF   e           k DISK
003300970414     F*--------
003400151001     FFNLSI4D   CF   E             WORKSTN
003500080918     F*--------
003601151124     Ftabel00f  IF   E           K DISK
003602151109     Ffnlbl01L  IF   E           K DISK
003603151109     Ffnlbl02L  IF   E           K DISK
003604151109     F                                     RENAME(FNLBL000:FNLBL2)
003605151112     Ffibsp01L  IF   E           K DISK
003606151112     Ffibsp02L  IF   E           K DISK    rename(fibsp000:fibsp02)
003607151124     ffnbld01l  if   e           k disk
003608151124     ffisgn05l  if   e           k disk
003700970410     D*--------------------------------------------------------------*
003800970410     D*  SCHIERE
003900970410     D*--------------------------------------------------------------*
006100970410     D* Caricamento £1
006300020506     D £1              S              3  0 DIM(30)
006400970414     D* ERRORI
006500151006     D ERR             S             78    DIM(29) CTDATA PERRCD(1)
006501150204
006505150204     d wdata           s              8  0
006507150204     D KTRC            S                   LIKE(AR4TRC)
006509150205     D x               S              5  0
006510150205     D wbsp            S              1    inz
006512150209     D wksc            S                   like(blpksc)
006513150209     D wlnp            S                   like(blplnp)
006516150217     d $Inzd02         s               n   inz(*on)
006517150217     d $Inzs01         s               n   inz(*on)
006518150217     d $Finerec        s               n   inz(*off)
006521150218     d wtfp            s                   LIKE(BLPTFP)
006522150217
006525150218     d Datadmy         s               d   datfmt(*dmy)
006526150219     d data_eur        s               D   datfmt(*eur)
006527151001     d savspe          s                   like(keyspe)
006528151124     d w_§1bfg6        s                   like(§1bfg6)
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
012401151006     D fnlsu3ds        DS
012402151006     D  ilsu3flg               1      1
012403151006     D  ilsu3aas               2      5
012404151006     D  ilsu3lnp               6      8
012405151006     D  ilsu3nrs               9     10
012406151006     D  ilsu3nsp              11     17
012407151006     D  ilsu3DSP_B            18     25
012408151006     D  ilsu3DSP_S            26     33
012409151006     D  ilsu3nos              34     34
012410151006     d
012411151006     d  ilsu3tcr              35     35
012412151006     d  ilsu3dcr              36     43
012413151006     d  ilsu3hcr              44     47
012414151006     D  olsu3err             100    101
012415151006     D  olsu3boagg           102    110
012416151006     D  olsu3bonagg          111    116
012417151006     D  olsu3msg             117    193
012418151006     D  olsu3sosp            194    194
012419151001     D keyspe          DS
012420151001     d  v1caas
012421151001     d  v1clnp
012422151001     d  v1cnrs
012423151001     d  v1cnsp
012424151001
013100060811     D OG143         E DS
013101151109     D A_og143       E DS                  EXTNAME(OG143)
013102151109     D                                     prefix(A_)
013103150204     D dbsp          E DS
013104151109     D dsbl4i        E DS
013105151124     D ds1b          E DS
014000040715
014100970410     D                SDS
014200970410     D  NOMPGM                 1     10
014201150211      /copy gaitrasrc/srcprotopr,tibs69r
014901150211
015000970410     C*--------------------------------------------------------------*
015100970410     C*  C I C L O     P R I N C I P A L E
015200970410     C*--------------------------------------------------------------*
015204150217
015300970410     C*  Inizializzo varibili videata
015400970410     C                   EXSR      INZ01
015500080421      *
015600970410     C*  Gestione della prima videata
015700970410     C     WFINE         DOWEQ     'N'
015701151001     C     WTPVID        CASEQ     '1'           GESD01
015702151001     C     WTPVID        CASEQ     '2'           GESD02
016100970611     C                   END
016101151001     C                   ENDdo
016200970410     C*  Fine pgm
016300970410     C                   SETON                                        LR
016400970611     C*--------------------------------------------------------------*
016500970611     C* INZ01: Impostazione della prima videata
016600970611     C*--------------------------------------------------------------*
016700970611     C     INZ01         BEGSR
016800970611     C*
016900970611     C                   SETOFF                                       2840
016901151001     C                   MOVEL     '1'           WTPVID
017200970616     C                   MOVEL     'N'           WFINE
017300970611     C                   MOVEL     NOMPGM        V1CPGM
017402150204     C                   clear                   V1clnp
017403150204     C                   clear                   V1cnrs
017404150204     C                   clear                   V1cnsp
017405150220     C                   z-add     2015          V1caas
017406151001     c                   clear                   savspe
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
019112151109     c                   eval      v2crmn=BLPrmn
019113151109     c                   eval      v2crma=BLPrma
019114150204     C* CERCO LA DESCRIZIONE AGGIUNTIVA PER IL DESTINATARIO
019117150204     C                   MOVEL     'D'           KTRC
019118150204     c                   clear                   ar4not
019119150204     C     Kar41         CHAIN     Fiar401L
019122150204     c                   eval      v2crsd=BLPRSd+ar4not
019123150204     c                   eval      v2cind=BLPINd
019124150204     c                   eval      v2ccad=BLPCAd
019125150204     c                   eval      v2clod=BLPLOd
019126150204     c                   eval      v2cprd=BLPPRd
019127150204     c                   eval      v2cnzd=BLPnzd
019128151109     C* CERCO il riferimento partner estero
019129151109     c                   setoff                                       13
019130151109     c                   clear                   v2crpe
019136151109     C                   MOVEL     'I'           KTRC
019137151109     c     Kar41         CHAIN     Fiar401L
019138151109     c                   if        %found(fiar401l)
019139151109     c                   eval      dsbl4i=ar4not
019140151109     c                   eval      v2crpe=§b4ipn
019141151109     c                   seton                                        13
019142151109     c                   else
019143151109     C                   MOVEL     'E'           KTRC
019144151109     c     Kar41         CHAIN     Fiar401L
019145151109     c                   if        %found(fiar401l)
019146151109     c                   movel     ar4not        v2crpe
019147151109     c                   seton                                        13
019148151109     c                   endif
019149151109     c                   endif
019152151109     c     blplna        chain     azorg01l
019153151109     c                   movel     orgde3        a_og143
019154151109     c                   if        not *in13
019155151109     c                             and (§ogntw='EEX' or §ogntw='FED'
019156151109     c                                              or §ogntw= 'DPD'
019157151109     c                                              or a_§ogntw='EEX'
019158151109     c                                              or a_§ogntw='FED'
019159151109     c                                              or a_§ogntw='DPD')
019160151109     c                   seton                                        13
019161151109     c                   endif
019162151109     c
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
021800151001     C                   SETOFF                                       4390
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
022901150217     C                   MOVEL     '2'           WTPVID
023000970410     C*
023100150203     C                   ENDSR
023200970611     C*--------------------------------------------------------------*
023300970611     C* GESD02: Gestione della seconda videata
023400970611     C*--------------------------------------------------------------*
023500970611     C     GESD02        BEGSR
023501150217     c*
023503150217     C                   EXSR      INZ02
023600970611     C*
023700970611     C* Eseguo exfmt della prima videata
023701150219     c                   do        *hival
023800970611     C                   EXFMT     LSI1D02
023900970611     C                   SETOFF                                       284041
024000970611     C                   SETOFF                                       424344
024001150209     C                   SETOFF                                       45
024100970611     C* Fine
024200151001     C     *INKC         IFEQ      '1'
024300151001     C                   MOVEL     'S'           WFINE             1
024301151001     c                   leavesr
024302151001     c                   endif
025000970611     C* Prima videata
025100970611     C     *INKL         IFEQ      '1'
025200970611     C                   MOVEL     '1'           WTPVID            1
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
030005151001     c                   exsr      sr_RiprBolla
030006150219     c                   if        not *in28
030007151006     c                   exsr      inz01
030010150204     C                   leavesr
030011150219     c                   endif
030012150219     c                   endif
030501150204     c                   endif
030502150219
030503150219     c                   enddo
030700970611     C*
030800150204     C                   ENDSR
047501150203     C*--------------------------------------------------------------*
047502150203     C* CTR01: Controlli prima videata
047503150203     C*--------------------------------------------------------------*
047504150203     C     CTR01         BEGSR
047505151001     C*
047506151001     C* Controllo se immesso anno
047507151001     C     V1CAAS        IFEQ      0
047508151001     C                   SETON                                        2840
047509151001     C                   MOVEL     ERR(5)        $MSG
047510151001     c                   leavesr
047511151001     C                   END
047512151001     C* Se anno digitato di 2 --> 4 cifre
047513151001     C                   MOVEL     V1CAAS        WSECOL            2 0
047514151001     C     WSECOL        IFEQ      0
047515151001     C     V1CAAS        IFGT      60
047516151001     C                   ADD       1900          V1CAAS
047517151001     C                   ELSE
047518151001     C                   ADD       2000          V1CAAS
047519151001     C                   END
047520151001     C                   END
047521151001     C* Controllo se immessa linea di partenza
047522151001     C     V1CLNP        IFEQ      0
047523151001     C                   SETON                                        2841
047524151001     C                   MOVEL     ERR(6)        $MSG
047525151001     c                   leavesr
047526151001     C                   END
047527151001     C* Controllo esistenza lnp
047528151001     C                   Z-ADD     V1CLNP        ORGFIL
047529151001     C     ORGFIL        CHAIN     AZORG01L
047530151001     C                   if        not %found(azorg01l)
047531151001     C                   SETON                                        2841
047532151001     C                   MOVEL     ERR(7)        $MSG
047533151001     c                   leavesr
047534151001     C                   END
047535151001     c                   movel     orgde3        og143
047536151001     C* Controllo se lnp in £1
047537151001     C     V1CLNP        LOOKUP    £1                                     32
047538151001     C     *IN32         IFEQ      '0'
047539151001     C                   SETON                                        2841
047540151001     C                   MOVEL     ERR(8)        $MSG
047541151001     c                   leavesr
047542151001     C                   END
047543151001     C* Controllo se immesso numero di spedizione
047544151001     C     V1CNSP        IFEQ      0
047545151001     C                   SETON                                        2843
047546151001     C                   MOVEL     ERR(9)        $MSG
047547151001     c                   leavesr
047548151001     C                   END
047549151001     C* Controllo se esiste spedizione
047550151001     C                   Z-ADD     V1CAAS        KAAS
047551151001     C                   Z-ADD     V1CLNP        KLNP
047552151001     C                   Z-ADD     V1CNRS        KNRS
047553151001     C                   Z-ADD     V1CNSP        KNSP
047554151001     C     KBLP          CHAIN(N)  FNBLP01L
047555151001     C                   if        not %found(fnblp01l)
047556151001     C                   SETON                                        2843
047557151001     C                   MOVEL     ERR(10)       $MSG
047558151001     c                   leavesr
047559151001     C                   END
047560151124     c* col ctm verifico se stampa segnacollo in differita
047561151124     c                   eval      cod='1B'
047562151124     c                   eval      key=blpctm
047563151124     c                   clear                   ds1b
047564151124     c                   clear                   w_§1bfg6
047565151124     c     ktab          chain     tabel00f
047566151124     c                   if        %found(tabel00f)
047567151124     c                   eval      ds1b=tbluni
047568151124     c                   eval      w_§1bfg6=§1bfg6
047569151124     c                   endif
047570151001     c                   exsr      ctr01_uni
047571151001     c                   if        *in28
047572151001     c                   seton                                        43
047573151001     c                   endif
047574150204     c                   endsr
068602150209     C*--------------------------------------------------------------*
068603150209     C*    Controlli unici per tutti i tipi di entrata
068604150209     C*--------------------------------------------------------------*
068605150209     C     ctr01_uni     BEGSR
068606151001     c* deve essere chiusa con CCA='7'-->Merce mai Affidata
068607151001     c                   if        blpcca<>'7'
068608151001     C                   SETON                                        28
068609151001     C                   MOVEL     ERR(01)       $MSG
068610151001     c                   leavesr
068611151001     c                   endif
068613151006     c* FT1 deve essere = 'N' cioè bolla chiusa in partenza
068614151006     c                   if        blpft1<>'N'
068615151006     c                   seton                                        28
068616151006     C                   MOVEL     ERR(13)       $MSG
068617151006     c                   leavesr
068618151006     c                   endif
068619151109     c* non deve essere un bolla legata
068620151109     C     KBLP          setll     FNLBL02L
068621151109     c     kblp          setll     fnlbl01l
068622151109     c                   if        %equal(fnlbl02l) or
068623151109     c                             %equal(fnlbl01l)
068624151109     C                   SETON                                        28
068625151109     C                   MOVEL     ERR(02)       $MSG
068626151109     c                   leavesr
068627151109     c                   endif
068628151109     c
068629151001     c* il cliente o la lnp della bolla deve essere in tabella BSP con
068630151001     c* la sospensione da spunte
068631151001     c                   z-add     blpksc        wksc
068632151001     c                   z-add     blplnp        wlnp
068633151001     c                   exsr      cer_bsp
068634151001     c                   if        wbsp=' '
068635151001     c                   seton                                        28
068636151001     C                   MOVEL     ERR(27)       $MSG
068637151001     c                   leavesr
068638151001     c                   endif
068653151124     c
068690151001     c* verifica se presenti spunte e avviso se non ce ne sono
068691151001     c                   if        savspe<>keyspe
068692150209     c                   clear                   wsispu            1
068693150209     c     kblt          setll     fnblt01l
068694150209     c     kblt          reade(n)  fnblt01l                               99
068695150209    2c                   dow       not *in99
068696150209     c
068697150219     c* Cerco una qualsiasi spunta
068698150209     c     kbrv          chain     fnbrv07l
068699150209    3c                   if        %found(fnbrv07l)
068700150209     c                   eval      wsispu='S'
068701150209     c                   seton                                        99
068702150209   x3c                   else
068703150209     c     kblt          reade(N)  fnblt01l                               99
068704150209    3c                   endif
068705150209    2c                   enddo
068706150209     c*
068707151001    2c                   if        wsispu=*blanks
068708151001     c                   eval      savspe=keyspe
068709150209     c                   seton                                        28
068710150209     C                   MOVEL     ERR(29)       $MSG
068711150209     c                   leavesr
068712150209     c                   endif
068713151001     c                   endif
068714151125     c* se prevista la stampa del segnacollo in differita
068715151125     c* deve essere presente il bld o il fisgn per poter stampare l'etichetta
068716151125     c* altrimenti errore
068717151125     c                   if        w_§1bfg6='D' and wsispu=*blanks
068718151125     c     kblp          setll     fnbld01l
068719151125     c                   if        not %equal(fnbld01l)
068720151125     c     kblp          setll     fisgn05l
068721151125     c                   if        not %equal(fisgn05l)
068722151125     c                   seton                                        28
068723151125     C                   MOVEL     ERR(14)       $MSG
068724151125     c                   leavesr
068725151125     c                   endif
068726151125     c                   endif
068727151125     c                   endif
068729150209     c
068743150209     c
068744150209     c                   endsr
068745150217      /free
069126150218       //--------------------------------------------------------------
069127150218       //?Cerca cliente in tabella BSP
069128150218       //--------------------------------------------------------------
069129150218       BEGSR cer_bsp;
069130150218       clear wbsp;
069131160125       // ksife=knsif;
069132160125       // setll (kcode: kline: ksife) tntbe02l;
069133160125       // if not %equal(tntbe02l);
069134160125       //    clear ksife;
069135160125       //    setll (kcode: kline: ksife) tntbe02l;
069136160125       // endif;
069137160125       // if %equal(tntbe02l);
069138160125       //    reade (kcode: kline: ksife) tntbe02l;
069139160125       //    dow not %eof(tntbe02l) and wbsp=' ';
069140160125       //       if tbeatb=' ' and (tbeke2=%editc(wksc:'X') or
069141160125       //                          tbeke2=%editc(wlnp:'X'));
069142160125       //          wbsp='S';
069143160125       //          dbsp=tbeuni;
069144160125       //       endif;
069145160125       //       reade (kcode: kline: ksife) tntbe02l;
069146160125       //    enddo;
069147160125       // endif;
069148151112       // Se ancora non trovato cerco in fibsp
069149151112       if wbsp=*blanks;
069150151112      /end-free
069151151112     c     wksc          setll     fibsp02l
069152151112     c                   if        %equal(fibsp02l)
069153151112     C                   eval      wbsp='S'
069155151112     c                   else
069156151112     c     wlnp          setll     fibsp01l
069157151112     c                   if        %equal(fibsp01l)
069158151112     C                   eval      wBsp='S'
069160151112     c                   endif
069161151112     c                   endif
069162151112      /free
069163151112       endif;
069164150218       endsr;
069165150217      /end-free
078601150204     C*--------------------------------------------------------------*
078602151001     C* Ripristino spedizione
078603150204     C*--------------------------------------------------------------*
078604151001     C     sr_RiprBolla  BEGSR
078605150219     C     KBLT          CHAIN(e)  FNBLP01L
078606150219     c                   if        %error
078607150219     c                   eval      $msg=err(11)
078608150219     c                   seton                                        28
078609150219     c                   leavesr
078610150219     c                   endif
078611150204     C     KBLT          CHAIN     FNBLT01L                           32
078612150204     C     *IN32         DOWEQ     '0'
078613151001     C                   clear                   BLTDFV
078614151001     C                   clear                   BLTNFV
078615151001     C                   clear                   BLTNPP
078616151001     C                   clear                   BLTDCM
078617151001     C                   clear                   BLTDTR
078618151001     C                   clear                   BLTFTR
078619151001     C                   clear                   BLTFL1
078620150204     C                   UPDATE    FNBLT000
078621150204     C* Lettura successiva
078622150204     C     KBLT          READE     FNBLT01L                               32
078623150204     C                   ENDdo
078624150204
078625150204     C                   Z-ADD     0             BLPDCE
078626150204     C                   Z-ADD     0             BLPNPI
078627150204     C                   Z-ADD     0             BLPNCI
078628150204     C                   Z-ADD     0             BLPNRC
078629151001     C                   clear                   BLPDBR
078630151001     C                   clear                   BLPDPC
078631151001     C                   clear                   BLPDUC
078632151001     C                   clear                   BLPDDC
078633151001     C                   clear                   BLPDCM
078634150204     C                   TIME                    WTIME             6 0
078635151001     C                   clear                   BLPHMC
078636151001     C                   clear                   BLPNFV
078637151001     C                   clear                   BLPNDC
078638151001     C                   clear                   BLPDT1
078639151001     C                   clear                   BLPDT2
078640151001     C                   clear                   BLPFT1
078641151001     C                   clear                   BLPFT2
078642150204     C* Per idd metto codice consegna anomala sulla bolla
078643151001     C                   clear                   BLPCCA
078650150204     C* devo inviare date consegna cliente
078651150303     C                   clear                   BLPDT3
078652150303     C                   clear                   BLPFT3
078653150204     C* Update bolla
078654150204     C                   UPDATE    FNBLP000
078655151006     c* richiamo pgm fnlsu3r per aggiornamento data spedizione e data ritiro
078656151006     c                   clear                   fnlsu3ds
078657151006     c                   eval      ilsu3aas=%editc(blpaas:'X')
078658151006     c                   eval      ilsu3lnp=%editc(blplnp:'X')
078659151006     c                   eval      ilsu3nrs=%editc(blpnrs:'X')
078660151006     c                   eval      ilsu3nsp=%editc(blpnsp:'X')
078661151006     c                   eval      wdata=(blpaas*10000)+blpmgs
078662151006     c                   eval      ilsu3dsp_B=%editc(wdata:'X')
078663151006     c                   eval      ilsu3dsp_S=%editc(oggi:'X')
078664151006     c                   eval      ilsu3nos='S'
078665151006     c                   eval      ilsu3flg='R'
078666151006     c                   movel     fnlsu3ds      kpjbu
078667151006     c                   call      'FNLSU3R'
078668151006     c                   parm                    kpjbA
078669151006     c                   movel     kpjbu         fnlsu3ds
078670150204     c                   endsr
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
082811150204     C     KBrv          KLIST
082812150204     C                   KFLD                    bltfls
082813150204     C                   KFLD                    bltlna
082814150204     C                   KFLD                    bltNRS
082815150204     C                   KFLD                    bltnsc
082816151124     c     ktab          klist
082817151124     C                   KFLD                    CODUT
082818151124     C                   KFLD                    COD               2
082819151124     C                   KFLD                    KEY               8
085900970410      *  Definizione variabili
086200970410     C     *LIKE         DEFINE    BLPAAS        KAAS
086300970410     C     *LIKE         DEFINE    BLPLNP        KLNP
086400970410     C     *LIKE         DEFINE    BLPNRS        KNRS
086500970410     C     *LIKE         DEFINE    BLPNSP        KNSP
087900970410      *
088000970410      *  Reperisco la data del giorno
088100970410     C                   TIME                    WHHDAT           14 0
088200970414     C                   MOVE      WHHDAT        G02DAT
088300970414     C                   MOVEL     *BLANK        G02ERR
088400970414     C                   CALL      'XSRDA8'
088500970414     C                   PARM                    WLBDA8
088600970414     C                   Z-ADD     G02INV        OGGI              8 0
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
102200151001La spedizione non è chiusa per "Merce Mai affidata"                           01
102300151109Bolla legata: non può essere ripristinata                                     02
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
103400151006Spedizione non ripristinabile: non è stata chiusa in partenza                 13
103500151124Spedizione non ripristinabile: mancano i dati per la stampa del segnacollo    14
103600151006                                                                              15
103700151006                                                                              16
103800151006                                                                              17
103900150220                                                                              18
104000150220                                                                              19
104100151006                                                                              20
104101151006
104300151006                                                                              22
104400150220                                                                              23
104500151006                                                                              24
104600150220                                                                              25
104700150220                                                                              26
104800150205Sped. non gestibile:non prevista dalla Sospensione Bolle x colli non spuntati 27
104900151006                                                                              28
105000151001Non presenti spunte per i colli della spedizione. Enter per Forzare           29
