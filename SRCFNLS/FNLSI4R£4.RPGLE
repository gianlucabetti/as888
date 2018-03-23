000100990610     H DECEDIT('0,') DATEDIT(*DMY.)
000200150202     H* FNLSI3R *----------------------------------------------------*
000300970410     H*                                                              *
000400151001     H*       - Ripristino bolle chiuse per merce mai affidata       *
000500050209     H*                                                              *
000600970410     H*--------------------------------------------------------------*
000700150205     FFNBLP01L  uF   E           K DISK
000800970410     F*--------
000900150204     FFNBLT01L  UF   E           K DISK
000901170224     FFiart01L  UF   E           K DISK
001000150204     FFNBrv07L  IF   E           K DISK
001100150204     FFiar401l  IF   E           K DISK
001200151001     F*--------
001300150219     FAZORG01L  IF   e           k DISK
001400970414     F*--------
001500151001     FFNLSI4D   CF   E             WORKSTN
001600080918     F*--------
001700151124     Ftabel00f  IF   E           K DISK
001800151109     Ffnlbl01L  IF   E           K DISK
001900151109     Ffnlbl02L  IF   E           K DISK
002000151109     F                                     RENAME(FNLBL000:FNLBL2)
002100151112     Ffibsp01L  IF   E           K DISK
002200151112     Ffibsp02L  IF   E           K DISK    rename(fibsp000:fibsp02)
002300151124     ffnbld01l  if   e           k disk
002400151124     ffisgn05l  if   e           k disk
002500970410     D*--------------------------------------------------------------*
002600970410     D*  SCHIERE
002700970410     D*--------------------------------------------------------------*
002800970410     D* Caricamento £1
002900020506     D £1              S              3  0 DIM(30)
003000970414     D* ERRORI
003100151006     D ERR             S             78    DIM(29) CTDATA PERRCD(1)
003200150204
003300150204     d wdata           s              8  0
003400150204     D KTRC            S                   LIKE(AR4TRC)
003500150205     D x               S              5  0
003600150205     D wbsp            S              1    inz
003700150209     D wksc            S                   like(blpksc)
003800150209     D wlnp            S                   like(blplnp)
003900150217     d $Inzd02         s               n   inz(*on)
004000150217     d $Inzs01         s               n   inz(*on)
004100150217     d $Finerec        s               n   inz(*off)
004200150218     d wtfp            s                   LIKE(BLPTFP)
004300150217
004400150218     d Datadmy         s               d   datfmt(*dmy)
004500150219     d data_eur        s               D   datfmt(*eur)
004600151001     d savspe          s                   like(keyspe)
004700151124     d w_§1bfg6        s                   like(§1bfg6)
004800970410     D*--------------------------------------------------------------*
004900970410     D* DS
005000970410     D*--------------------------------------------------------------*
005100020506     d trul06ds      e ds
005200020506     d  lin                    1     90  0 dim(30)
005300150218     d fnlv55ds      e ds
005400970414     D WLBDA8          DS
005500970414     D  G02DAT                 1      8  0
005600150203     D  G02INV                 9     16  0
005700970414     D  G02ERR                17     17
005800970414     D  G02TGI                18     22  0
005900080421      *
006000970410     D KPJBA         E DS
006100150217     d fnblpds       e ds                  extname(fnblp00f) inz
006200150204       // -?Parametri x Controllo profilo utenti?
006300150204     d TIBS34ds      e ds
006400150211     d TIBS69DS      E DS                  INZ
006500150211     d DS_cnaco      E DS                  extname(CNACO00F)
006600150211     d DS_cnind      E DS                  extname(CNIND00F)
006700150211     d DS_cnclp      E DS                  extname(CNCLP00F)
006800150211     d DS_fncls      E DS                  extname(FNCLS00F)
006900150204       // -?Ds di riferimento al file esterno AZUTE00F?
007000150204     d AZUTEds       e ds                  extname(AZUTE00F)
007100150204       // -?Ds per dati organigramma?
007200150204     d dDATIUTE      e ds
007201161108     d fnlru6ds      e ds
007202161108      * ?Parametri pgm interrogaz. Bolle di Filiale FNLRU6R
007203161108     D PARAMu6ds       DS
007204161108     d* CAMPI DI OUTPUT:
007205161108     d* . Key spedizione
007206161108     D  pa1AAS                14     17  0
007207161108     D  pa1LNP                18     20  0
007208161108     D  pa1NRS                21     22  0
007209161108     D  pa1NSP                23     29  0
007210161108     d* . Flag '1'=Premuto F3=Fine
007211161108     D  PA1F03                30     30
007212161108     d* CAMPI DI INPUT:
007213161108     d* . Modalità di richiamo
007214161108     D  PA1FLG                31     31
007215161108     D* . Flag '1'= RICH DA PGM GIAC. (Serve per disabilitare tasto funzionale F
007216161108     d*                                in interrogazione della bolla ed evitare
007217161108     d*                                errore di chiamata ricorsiva
007218161108     D  PA1GIA               144    144
007219161108     d* . Solo dal chiamante FNLRU6R1 per il Perfect Order Wurth:
007220161108     d  pa1rma               149    163
007221161108     d  pa1xco               164    164
007222161108     d* non utilizzato
007223161108     d  pa1flo               165    256
007224161108      * ?Parametri pgm interrogaz. Bolle di Filiale FNLRU6R1
007225161108     D PARAMU6ds1      DS
007226161108     d** INPUT/OUTPUT (se richiesta la selezione bolla)
007227161108     D  pa1AAS_               14     17  0
007228161108     D  pa1LNP_               18     20  0
007229161108     D  pa1NRS_               21     22  0
007230161108     D  pa1NSP_               23     29  0
007231161108     d** OUTPUT
007232161108     D  PA1F03_               30     30
007233161108     d** INPUT
007234161108     D  PA1FLG_               31     31
007235161108     D*    SE RICH DA PGM GIAC. = 1
007236161108     D  PA1GIA_              144    144
007237161108     d** OUTPUT
007238161108     d  pa1rma_              149    163
007239161108     d  pa1xco_              164    164
007240161108     d** INPUT
007241161108     d*   Tipo lancio: "C" -           CHIUSO CON LR
007242161108     d*                "L" - ELABORO E CHIUDO CON LR
007243161108     d*                " " - ELABORO E CHIUDO IN RETRN
007244161108     d  pa1tla_              165    165
007245161108     d  pa1flo_              166    256
009500151006     D fnlsu3ds        DS
009600151006     D  ilsu3flg               1      1
009700151006     D  ilsu3aas               2      5
009800151006     D  ilsu3lnp               6      8
009900151006     D  ilsu3nrs               9     10
010000151006     D  ilsu3nsp              11     17
010100151006     D  ilsu3DSP_B            18     25
010200151006     D  ilsu3DSP_S            26     33
010300151006     D  ilsu3nos              34     34
010400151006     d
010500151006     d  ilsu3tcr              35     35
010600151006     d  ilsu3dcr              36     43
010700151006     d  ilsu3hcr              44     47
010800151006     D  olsu3err             100    101
010900151006     D  olsu3boagg           102    110
011000151006     D  olsu3bonagg          111    116
011100151006     D  olsu3msg             117    193
011200151006     D  olsu3sosp            194    194
011300151001     D keyspe          DS
011400151001     d  v1caas
011500151001     d  v1clnp
011600151001     d  v1cnrs
011700151001     d  v1cnsp
011800151001
011900060811     D OG143         E DS
012000151109     D A_og143       E DS                  EXTNAME(OG143)
012100151109     D                                     prefix(A_)
012200150204     D dbsp          E DS
012300151109     D dsbl4i        E DS
012400151124     D ds1b          E DS
012500040715
012600970410     D                SDS
012700970410     D  NOMPGM                 1     10
012800150211      /copy gaitrasrc/srcprotopr,tibs69r
012900150211
013000970410     C*--------------------------------------------------------------*
013100970410     C*  C I C L O     P R I N C I P A L E
013200970410     C*--------------------------------------------------------------*
013300150217
013400970410     C*  Inizializzo varibili videata
013500970410     C                   EXSR      INZ01
013600080421      *
013700970410     C*  Gestione della prima videata
013800970410     C     WFINE         DOWEQ     'N'
013900151001     C     WTPVID        CASEQ     '1'           GESD01
014000151001     C     WTPVID        CASEQ     '2'           GESD02
014100970611     C                   END
014200151001     C                   ENDdo
014300970410     C*  Fine pgm
014301161114     c                   eval      pa1tla_='C'
014302161114     c                   eval      kpjbu=paramu6ds1
014303161114     c                   call      'FNLRU6R1'
014304161114     c                   parm                    kpjba
014305161114
014400970410     C                   SETON                                        LR
014500970611     C*--------------------------------------------------------------*
014600970611     C* INZ01: Impostazione della prima videata
014700970611     C*--------------------------------------------------------------*
014800970611     C     INZ01         BEGSR
014900970611     C*
015000970611     C                   SETOFF                                       2840
015100151001     C                   MOVEL     '1'           WTPVID
015200970616     C                   MOVEL     'N'           WFINE
015300970611     C                   MOVEL     NOMPGM        V1CPGM
015400150204     C                   clear                   V1clnp
015500150204     C                   clear                   V1cnrs
015600150204     C                   clear                   V1cnsp
015700160330     C****               z-add     2015          V1caas
015800160330     C                   z-add     UYEAR         V1CAAS
015900160330     c     uyear         iflt      60
016000160330     c                   movel     20            v1caas
016100160330     c                   else
016200160330     C                   MOVEL     19            V1CAAS
016300160330     c                   endif
016400151001     c                   clear                   savspe
016500970611     C*
016600970611     C                   ENDSR
016700970410     C*--------------------------------------------------------------*
016800970611     C* INZ02: Impostazione della prima videata
016900970410     C*--------------------------------------------------------------*
017000970611     C     INZ02         BEGSR
017100970410     C*
017200970410     C                   SETOFF                                       284041
017300970611     C                   SETOFF                                       424344
017400150209     C                   SETOFF                                       45
017500970611     C                   MOVEL     NOMPGM        V2CPGM
017600150204     C                   z-add     blpksc        V2CKSC
017700150204     C                   MOVEL     blprsm        V2DKSC
017800150204     c                   z-add     blplnp        V2clnp
017900150204     C                   z-add     blpnrs        V2Cnrs
018000150204     C                   z-add     blpnsp        v2cnsp
018100150204     C                   z-add     blpaas        V2Caas
018200150204     c     *iso          move      blpdim        data_eur
018300150204     c                   move      data_eur      v2cdim
018400150204     c                   eval      wdata=(blpaas*10000)+blpmgs
018500150204     c     *iso          move      wdata         data_eur
018600150204     c                   move      data_eur      v2cdsp
018700150204     c                   eval      v2crsm=BLPRSM
018800150204     c                   eval      v2cinm=BLPINM
018900150204     c                   eval      v2ccam=BLPCAM
019000150204     c                   eval      v2clom=BLPLOM
019100150204     c                   eval      v2cprm=BLPPRM
019200150204     c                   eval      v2cnzm=BLPnzm
019300151109     c                   eval      v2crmn=BLPrmn
019400151109     c                   eval      v2crma=BLPrma
019500150204     C* CERCO LA DESCRIZIONE AGGIUNTIVA PER IL DESTINATARIO
019600150204     C                   MOVEL     'D'           KTRC
019700150204     c                   clear                   ar4not
019800150204     C     Kar41         CHAIN     Fiar401L
019900150204     c                   eval      v2crsd=BLPRSd+ar4not
020000150204     c                   eval      v2cind=BLPINd
020100150204     c                   eval      v2ccad=BLPCAd
020200150204     c                   eval      v2clod=BLPLOd
020300150204     c                   eval      v2cprd=BLPPRd
020400150204     c                   eval      v2cnzd=BLPnzd
020500151109     C* CERCO il riferimento partner estero
020600151109     c                   setoff                                       13
020700151109     c                   clear                   v2crpe
020800151109     C                   MOVEL     'I'           KTRC
020900151109     c     Kar41         CHAIN     Fiar401L
021000151109     c                   if        %found(fiar401l)
021100151109     c                   eval      dsbl4i=ar4not
021200151109     c                   eval      v2crpe=§b4ipn
021300151109     c                   seton                                        13
021400151109     c                   else
021500151109     C                   MOVEL     'E'           KTRC
021600151109     c     Kar41         CHAIN     Fiar401L
021700151109     c                   if        %found(fiar401l)
021800151109     c                   movel     ar4not        v2crpe
021900151109     c                   seton                                        13
022000151109     c                   endif
022100151109     c                   endif
022200151109     c     blplna        chain     azorg01l
022300151109     c                   movel     orgde3        a_og143
022400151109     c                   if        not *in13
022500151109     c                             and (§ogntw='EEX' or §ogntw='FED'
022600151109     c                                              or §ogntw= 'DPD'
022700151109     c                                              or a_§ogntw='EEX'
022800151109     c                                              or a_§ogntw='FED'
022900151109     c                                              or a_§ogntw='DPD')
023000151109     c                   seton                                        13
023100151109     c                   endif
023200151109     c
023300970410     C*
023400970410     C                   ENDSR
023500970410     C*--------------------------------------------------------------*
023600970410     C* GESD01: Gestione della prima videata
023700970410     C*--------------------------------------------------------------*
023800970410     C     GESD01        BEGSR
023900970410     C*
024000970410     C* Eseguo exfmt della prima videata
024100970410     C                   EXFMT     LSI1D01
024200150203     C                   SETOFF                                       284041
024300151001     C                   SETOFF                                       4390
024400970410     C* Fine
024500970410     C     *INKC         IFEQ      '1'
024600970414     C                   MOVEL     'S'           WFINE             1
024700150205     C                   leavesr
024800970410     C                   END
024900150204     C* Interrogazione bolle
025000150204     C     *INKG         IFEQ      '1'
025100150204     C     V1CNSP        IFGT      0
025101161108     C                   CLEAR                   paramu6ds1
025102161108     C                   Z-ADD     V1CAAS        pa1AAS_
025103161108     C                   Z-ADD     V1CLNP        Pa1LNP_
025104161108     C                   Z-ADD     V1CNRS        Pa1NRS_
025105161108     C                   Z-ADD     V1CNSP        Pa1NSP_
025106161108     C                   eval      pa1flg_='2'
025107161108     C                   MOVEL     paramu6ds1    KPJBU
025108161108     C                   CALL      'FNLRU6R1'
025109161108     C                   PARM                    KPJBA
025110161108     C                   MOVEL     KPJBU         PARAmu6ds1
025111161108     C     Pa1F03_       IFEQ      *BLANKS
025112161108     C                   Z-ADD     Pa1AAS_       v1CAAS
025113161108     C                   Z-ADD     Pa1LNP_       v1CLNP
025114161108     C                   Z-ADD     Pa1NRS_       v1CNRS
025115161108     C                   Z-ADD     Pa1NSP_       v1CNSP
025116161108     C                   ENDIF
026800150204     C*
026900150204     C                   ELSE
027000150204     C*
027001161108     C                   CLEAR                   PARAmu6ds
027002161108     C                   MOVEL     '2'           Pa1FLG
027003161108     C                   MOVEL     PARAmu6ds     KPJBU
027004161108     c                   clear                   fnlru6ds
027005161108     c                   eval      ILRU6SSP='N'
027007161108     C                   CALL      'FNLRU6R'
027008161108     C                   PARM                    KPJBA
027009161108     c                   parm                    fnlru6ds
027010161108     C                   MOVEL     KPJBU         PARAMu6ds
027011161108     C     Pa1F03        IFEQ      *BLANKS
027012161108     C                   Z-ADD     Pa1AAS        V1CAAS
027013161108     C                   Z-ADD     Pa1LNP        V1CLNP
027014161108     C                   Z-ADD     Pa1NRS        V1CNRS
027015161108     C                   Z-ADD     Pa1NSP        V1CNSP
027016161108     C                   ENDIF
028300150204     C                   ENDIF
028400150204     C                   leavesr
028500150204     C                   END
028600970410     C* Controlli
028700970410     C                   EXSR      CTR01
028800150219     c   90
028900150219     Cor 28              leavesr
029000970611     C*
029100150217     C                   MOVEL     '2'           WTPVID
029200970410     C*
029300150203     C                   ENDSR
029400970611     C*--------------------------------------------------------------*
029500970611     C* GESD02: Gestione della seconda videata
029600970611     C*--------------------------------------------------------------*
029700970611     C     GESD02        BEGSR
029800150217     c*
029900150217     C                   EXSR      INZ02
030000970611     C*
030100970611     C* Eseguo exfmt della prima videata
030200150219     c                   do        *hival
030300970611     C                   EXFMT     LSI1D02
030400970611     C                   SETOFF                                       284041
030500970611     C                   SETOFF                                       424344
030600150209     C                   SETOFF                                       45
030700970611     C* Fine
030800151001     C     *INKC         IFEQ      '1'
030900151001     C                   MOVEL     'S'           WFINE             1
031000151001     c                   leavesr
031100151001     c                   endif
031200970611     C* Prima videata
031300970611     C     *INKL         IFEQ      '1'
031400970611     C                   MOVEL     '1'           WTPVID            1
031500150204     C                   leavesr
031600970611     C                   END
031700080421      *
031800970611     C* Controlli
031900150204     C***                EXSR      CTR02
032000150204     C***28              GOTO      FINVD2
032100970611     C*
032200150204     c                   if        *inkf
032300150219     c     kblt          chain(n)  fnblp01l
032400150219     c                   exsr      ctr01_uni
032500150219     c                   if        not *in28
032600151001     c                   exsr      sr_RiprBolla
032700150219     c                   if        not *in28
032800151006     c                   exsr      inz01
032900150204     C                   leavesr
033000150219     c                   endif
033100150219     c                   endif
033200150204     c                   endif
033300150219
033400150219     c                   enddo
033500970611     C*
033600150204     C                   ENDSR
033700150203     C*--------------------------------------------------------------*
033800150203     C* CTR01: Controlli prima videata
033900150203     C*--------------------------------------------------------------*
034000150203     C     CTR01         BEGSR
034100151001     C*
034200151001     C* Controllo se immesso anno
034300151001     C     V1CAAS        IFEQ      0
034400151001     C                   SETON                                        2840
034500151001     C                   MOVEL     ERR(5)        $MSG
034600151001     c                   leavesr
034700151001     C                   END
034800151001     C* Se anno digitato di 2 --> 4 cifre
034900151001     C                   MOVEL     V1CAAS        WSECOL            2 0
035000151001     C     WSECOL        IFEQ      0
035100151001     C     V1CAAS        IFGT      60
035200151001     C                   ADD       1900          V1CAAS
035300151001     C                   ELSE
035400151001     C                   ADD       2000          V1CAAS
035500151001     C                   END
035600151001     C                   END
035700151001     C* Controllo se immessa linea di partenza
035800151001     C     V1CLNP        IFEQ      0
035900151001     C                   SETON                                        2841
036000151001     C                   MOVEL     ERR(6)        $MSG
036100151001     c                   leavesr
036200151001     C                   END
036300151001     C* Controllo esistenza lnp
036400151001     C                   Z-ADD     V1CLNP        ORGFIL
036500151001     C     ORGFIL        CHAIN     AZORG01L
036600151001     C                   if        not %found(azorg01l)
036700151001     C                   SETON                                        2841
036800151001     C                   MOVEL     ERR(7)        $MSG
036900151001     c                   leavesr
037000151001     C                   END
037100151001     c                   movel     orgde3        og143
037200151001     C* Controllo se lnp in £1
037300151001     C     V1CLNP        LOOKUP    £1                                     32
037400151001     C     *IN32         IFEQ      '0'
037500151001     C                   SETON                                        2841
037600151001     C                   MOVEL     ERR(8)        $MSG
037700151001     c                   leavesr
037800151001     C                   END
037900151001     C* Controllo se immesso numero di spedizione
038000151001     C     V1CNSP        IFEQ      0
038100151001     C                   SETON                                        2843
038200151001     C                   MOVEL     ERR(9)        $MSG
038300151001     c                   leavesr
038400151001     C                   END
038500151001     C* Controllo se esiste spedizione
038600151001     C                   Z-ADD     V1CAAS        KAAS
038700151001     C                   Z-ADD     V1CLNP        KLNP
038800151001     C                   Z-ADD     V1CNRS        KNRS
038900151001     C                   Z-ADD     V1CNSP        KNSP
039000151001     C     KBLP          CHAIN(N)  FNBLP01L
039100151001     C                   if        not %found(fnblp01l)
039200151001     C                   SETON                                        2843
039300151001     C                   MOVEL     ERR(10)       $MSG
039400151001     c                   leavesr
039500151001     C                   END
039600151124     c* col ctm verifico se stampa segnacollo in differita
039700151124     c                   eval      cod='1B'
039800151124     c                   eval      key=blpctm
039900151124     c                   clear                   ds1b
040000151124     c                   clear                   w_§1bfg6
040100151124     c     ktab          chain     tabel00f
040200151124     c                   if        %found(tabel00f)
040300151124     c                   eval      ds1b=tbluni
040400151124     c                   eval      w_§1bfg6=§1bfg6
040500151124     c                   endif
040600151001     c                   exsr      ctr01_uni
040700151001     c                   if        *in28
040800151001     c                   seton                                        43
040900151001     c                   endif
041000150204     c                   endsr
041100150209     C*--------------------------------------------------------------*
041200150209     C*    Controlli unici per tutti i tipi di entrata
041300150209     C*--------------------------------------------------------------*
041400150209     C     ctr01_uni     BEGSR
041500151001     c* deve essere chiusa con CCA='7'-->Merce mai Affidata
041600151001     c                   if        blpcca<>'7'
041700151001     C                   SETON                                        28
041800151001     C                   MOVEL     ERR(01)       $MSG
041900151001     c                   leavesr
042000151001     c                   endif
042100151006     c* FT1 deve essere = 'N' cioè bolla chiusa in partenza
042200151006     c                   if        blpft1<>'N'
042300151006     c                   seton                                        28
042400151006     C                   MOVEL     ERR(13)       $MSG
042500151006     c                   leavesr
042600151006     c                   endif
042700151109     c* non deve essere un bolla legata
042800151109     C     KBLP          setll     FNLBL02L
042900151109     c     kblp          setll     fnlbl01l
043000151109     c                   if        %equal(fnlbl02l) or
043100151109     c                             %equal(fnlbl01l)
043200151109     C                   SETON                                        28
043300151109     C                   MOVEL     ERR(02)       $MSG
043400151109     c                   leavesr
043500151109     c                   endif
043600151109     c
043700151001     c* il cliente o la lnp della bolla deve essere in tabella BSP con
043800151001     c* la sospensione da spunte
043900151001     c                   z-add     blpksc        wksc
044000151001     c                   z-add     blplnp        wlnp
044100151001     c                   exsr      cer_bsp
044200151001     c                   if        wbsp=' '
044300151001     c                   seton                                        28
044400151001     C                   MOVEL     ERR(27)       $MSG
044500151001     c                   leavesr
044600151001     c                   endif
044700151124     c
044800151001     c* verifica se presenti spunte e avviso se non ce ne sono
044900151001     c                   if        savspe<>keyspe
045000150209     c                   clear                   wsispu            1
045100150209     c     kblt          setll     fnblt01l
045200150209     c     kblt          reade(n)  fnblt01l                               99
045300150209    2c                   dow       not *in99
045400150209     c
045500150219     c* Cerco una qualsiasi spunta
045600150209     c     kbrv          chain     fnbrv07l
045700150209    3c                   if        %found(fnbrv07l)
045800150209     c                   eval      wsispu='S'
045900150209     c                   seton                                        99
046000150209   x3c                   else
046100150209     c     kblt          reade(N)  fnblt01l                               99
046200150209    3c                   endif
046300150209    2c                   enddo
046400150209     c*
046500151001    2c                   if        wsispu=*blanks
046600151001     c                   eval      savspe=keyspe
046700150209     c                   seton                                        28
046800150209     C                   MOVEL     ERR(29)       $MSG
046900150209     c                   leavesr
047000150209     c                   endif
047100151001     c                   endif
047200151125     c* se prevista la stampa del segnacollo in differita
047300151125     c* deve essere presente il bld o il fisgn per poter stampare l'etichetta
047400151125     c* altrimenti errore
047500151125     c                   if        w_§1bfg6='D' and wsispu=*blanks
047600151125     c     kblp          setll     fnbld01l
047700151125     c                   if        not %equal(fnbld01l)
047800151125     c     kblp          setll     fisgn05l
047900151125     c                   if        not %equal(fisgn05l)
048000151125     c                   seton                                        28
048100151125     C                   MOVEL     ERR(14)       $MSG
048200151125     c                   leavesr
048300151125     c                   endif
048400151125     c                   endif
048500151125     c                   endif
048600150209     c
048700150209     c
048800150209     c                   endsr
048900150217      /free
049000150218       //--------------------------------------------------------------
049100150218       //?Cerca cliente in tabella BSP
049200150218       //--------------------------------------------------------------
049300150218       BEGSR cer_bsp;
049400150218       clear wbsp;
049500160125       // ksife=knsif;
049600160125       // setll (kcode: kline: ksife) tntbe02l;
049700160125       // if not %equal(tntbe02l);
049800160125       //    clear ksife;
049900160125       //    setll (kcode: kline: ksife) tntbe02l;
050000160125       // endif;
050100160125       // if %equal(tntbe02l);
050200160125       //    reade (kcode: kline: ksife) tntbe02l;
050300160125       //    dow not %eof(tntbe02l) and wbsp=' ';
050400160125       //       if tbeatb=' ' and (tbeke2=%editc(wksc:'X') or
050500160125       //                          tbeke2=%editc(wlnp:'X'));
050600160125       //          wbsp='S';
050700160125       //          dbsp=tbeuni;
050800160125       //       endif;
050900160125       //       reade (kcode: kline: ksife) tntbe02l;
051000160125       //    enddo;
051100160125       // endif;
051200151112       // Se ancora non trovato cerco in fibsp
051300151112       if wbsp=*blanks;
051400151112      /end-free
051500151112     c     wksc          setll     fibsp02l
051600151112     c                   if        %equal(fibsp02l)
051700151112     C                   eval      wbsp='S'
051800151112     c                   else
051900151112     c     wlnp          setll     fibsp01l
052000151112     c                   if        %equal(fibsp01l)
052100151112     C                   eval      wBsp='S'
052200151112     c                   endif
052300151112     c                   endif
052400151112      /free
052500151112       endif;
052600150218       endsr;
052700150217      /end-free
052800150204     C*--------------------------------------------------------------*
052900151001     C* Ripristino spedizione
053000150204     C*--------------------------------------------------------------*
053100151001     C     sr_RiprBolla  BEGSR
053200150219     C     KBLT          CHAIN(e)  FNBLP01L
053300150219     c                   if        %error
053400150219     c                   eval      $msg=err(11)
053500150219     c                   seton                                        28
053600150219     c                   leavesr
053700150219     c                   endif
053800150204     C     KBLT          CHAIN     FNBLT01L                           32
053900150204     C     *IN32         DOWEQ     '0'
054000151001     C                   clear                   BLTDFV
054100151001     C                   clear                   BLTNFV
054200151001     C                   clear                   BLTNPP
054300151001     C                   clear                   BLTDCM
054400151001     C                   clear                   BLTDTR
054500151001     C                   clear                   BLTFTR
054600151001     C                   clear                   BLTFL1
054700150204     C                   UPDATE    FNBLT000
054800150204     C* Lettura successiva
054900150204     C     KBLT          READE     FNBLT01L                               32
055000150204     C                   ENDdo
055100150204
055101170224     C     KBLT          CHAIN     Fiart01L                           32
055102170224     C     *IN32         DOWEQ     '0'
055103170224     C                   clear                   artDFV
055104170224     C                   clear                   artNFV
055105170224     C                   clear                   artNPP
055106170224     C                   clear                   artDCM
055107170224     C                   clear                   artdtc
055108170224     C                   clear                   artcca
055110170224     C                   UPDATE    Fiart000
055111170224     C* Lettura successiva
055112170224     C     KBLT          READE     Fiart01L                               32
055113170224     C                   ENDdo
055114170224
055200150204     C                   Z-ADD     0             BLPDCE
055300150204     C                   Z-ADD     0             BLPNPI
055400150204     C                   Z-ADD     0             BLPNCI
055500150204     C                   Z-ADD     0             BLPNRC
055600151001     C                   clear                   BLPDBR
055700151001     C                   clear                   BLPDPC
055800151001     C                   clear                   BLPDUC
055900151001     C                   clear                   BLPDDC
056000151001     C                   clear                   BLPDCM
056100150204     C                   TIME                    WTIME             6 0
056200151001     C                   clear                   BLPHMC
056300151001     C                   clear                   BLPNFV
056400151001     C                   clear                   BLPNDC
056500151001     C                   clear                   BLPDT1
056600151001     C                   clear                   BLPDT2
056700151001     C                   clear                   BLPFT1
056800151001     C                   clear                   BLPFT2
056900150204     C* Per idd metto codice consegna anomala sulla bolla
057000151001     C                   clear                   BLPCCA
057100150204     C* devo inviare date consegna cliente
057200150303     C                   clear                   BLPDT3
057300150303     C                   clear                   BLPFT3
057400150204     C* Update bolla
057500150204     C                   UPDATE    FNBLP000
057600151006     c* richiamo pgm fnlsu3r per aggiornamento data spedizione e data ritiro
057700151006     c                   clear                   fnlsu3ds
057800151006     c                   eval      ilsu3aas=%editc(blpaas:'X')
057900151006     c                   eval      ilsu3lnp=%editc(blplnp:'X')
058000151006     c                   eval      ilsu3nrs=%editc(blpnrs:'X')
058100151006     c                   eval      ilsu3nsp=%editc(blpnsp:'X')
058200151006     c                   eval      wdata=(blpaas*10000)+blpmgs
058300151006     c                   eval      ilsu3dsp_B=%editc(wdata:'X')
058400151006     c                   eval      ilsu3dsp_S=%editc(oggi:'X')
058500151006     c                   eval      ilsu3nos='S'
058600151006     c                   eval      ilsu3flg='R'
058700151006     c                   movel     fnlsu3ds      kpjbu
058800151006     c                   call      'FNLSU3R'
058900151006     c                   parm                    kpjbA
059000151006     c                   movel     kpjbu         fnlsu3ds
059100150204     c                   endsr
059200970410     C*--------------------------------------------------------------*
059300970410     C* *INZSR: Operazioni iniziali
059400970410     C*--------------------------------------------------------------*
059500970410     C     *INZSR        BEGSR
059600970410     C*
059700970410     C     *ENTRY        PLIST
059800970410     C                   PARM                    KPJBA
059900150204
060000150204     c     *dtaara       define    §azute        AZUTEds
060100150204     c     *dtaara       define    §datiute      dDATIUTE
060200150204     c                   in(E)     *dtaara
060300150204     c                   if        %ERROR or RSUT = *blanks
060400150204     c                   clear                   Tibs34Ds
060500150204     c                   call      'TIBS34R'
060600150204     c                   parm                    Tibs34Ds
060700150204     c                   in        *dtaara
060800150204     c                   endif
060900150204      *
061000150204     c                   z-add     1             CodUt             1 0
061100030924      *
061200030924      *
061300970410      *  Definizione chiavi
061400970410     C     KBLP          KLIST
061500970410     C                   KFLD                    KAAS
061600970410     C                   KFLD                    KLNP
061700970410     C                   KFLD                    KNRS
061800970410     C                   KFLD                    KNSP
061900970410     C     KBLT          KLIST
062000150204     C                   KFLD                    blpaas
062100150204     C                   KFLD                    blplnp
062200150204     C                   KFLD                    blpnrs
062300150204     C                   KFLD                    blpnsp
062400150204     C     Kar41         KLIST
062500150204     C                   KFLD                    blpaas
062600150204     C                   KFLD                    blplnp
062700150204     C                   KFLD                    blpnrs
062800150204     C                   KFLD                    blpnsp
062900150204     C                   KFLD                    ktrc
063000150204     C     KBrv          KLIST
063100150204     C                   KFLD                    bltfls
063200150204     C                   KFLD                    bltlna
063300150204     C                   KFLD                    bltNRS
063400150204     C                   KFLD                    bltnsc
063500151124     c     ktab          klist
063600151124     C                   KFLD                    CODUT
063700151124     C                   KFLD                    COD               2
063800151124     C                   KFLD                    KEY               8
063900970410      *  Definizione variabili
064000970410     C     *LIKE         DEFINE    BLPAAS        KAAS
064100970410     C     *LIKE         DEFINE    BLPLNP        KLNP
064200970410     C     *LIKE         DEFINE    BLPNRS        KNRS
064300970410     C     *LIKE         DEFINE    BLPNSP        KNSP
064400970410      *
064500970410      *  Reperisco la data del giorno
064600970410     C                   TIME                    WHHDAT           14 0
064700970414     C                   MOVE      WHHDAT        G02DAT
064800970414     C                   MOVEL     *BLANK        G02ERR
064900970414     C                   CALL      'XSRDA8'
065000970414     C                   PARM                    WLBDA8
065100970414     C                   Z-ADD     G02INV        OGGI              8 0
065200150204     C***
065300970410     C* Eseguo caricamento £1
065400020506     c                   clear                   trul06ds
065500020506     c                   eval      d06cod = '£1'
065600150205     c                   movel     dutpou        d06key
065700020506     c                   movel     trul06ds      kpjbu
065800020506     c                   call      'TRUL06R'
065900020506     c                   parm                    kpjba
066000020506     c                   movel     kpjbu         trul06ds
066100020506     c                   movea     lin           £1
066200150218     c* terminal di partenza del p.o. utente
066300150218     c                   clear                   fnlv55ds
066400150218     C                   MOVEL     'P'           D55TPT
066500150218     C                   MOVEL     dutpou        D55LIN
066600150218     C                   z-add     oggi          D55drf
066700150218     C                   CALL      'FNLV55R'
066800150218     C                   PARM                    fnlv55ds
066900150218     c                   z-add     d55tfp        wtfp
067000020912      *
067100970410     C*
067200970410     C                   ENDSR
067300080930      *------------------------------------------------------------------------*
067400080930      * Routine di controllo x eventuali Errori
067500080930      *------------------------------------------------------------------------*
067600080930     C     *PSSR         BEGSR
067700080930      *
067800080930     C                   ENDSR     '*CANCL'
067900080930     C*------------------------------------------------------------------------*
068000971008**
068100151001La spedizione non è chiusa per "Merce Mai affidata"                           01
068200151109Bolla legata: non può essere ripristinata                                     02
068300150220                                                                              03
068400150220                                                                              04
068500970701Immettere anno della spedizione                                               05
068600970701Immettere linea di partenza della spedizione                                  06
068700970701Linea di partenza inesistente                                                 07
068800150209Spedizione non in gestione alla filiale                                       08
068900970701Immettere numero della spedizione                                             09
069000970701Spedizione inesistente                                                        10
069100150219Richiesta non elaborabile: bolla allocata da altro utente. Attendi e riprova  11
069200980206Bolla figlia: impossibile gestirla                                            12
069300151006Spedizione non ripristinabile: non è stata chiusa in partenza                 13
069400151124Spedizione non ripristinabile: mancano i dati per la stampa del segnacollo    14
069500151006                                                                              15
069600151006                                                                              16
069700151006                                                                              17
069800150220                                                                              18
069900150220                                                                              19
070000151006                                                                              20
070100151006
070200151006                                                                              22
070300150220                                                                              23
070400151006                                                                              24
070500150220                                                                              25
070600150220                                                                              26
070700150205Sped. non gestibile:non prevista dalla Sospensione Bolle x colli non spuntati 27
070800151006                                                                              28
070900151001Non presenti spunte per i colli della spedizione. Enter per Forzare           29
