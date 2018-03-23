000100990610     H DECEDIT('0,') DATEDIT(*DMY.)
000200970410     H* FNLSI1R *----------------------------------------------------*
000300970410     H*                                                              *
000400970611     H*       - CHIUSURA BOLLA IN PARTENZA CON PRATICA IDD      -    *
000500050209     H*                                                              *
000600970410     H*--------------------------------------------------------------*
000700990810     FFNEVB04L  IF   E           K DISK
000800980204     F*--------
000900970612     FFNBLP01L  IF   E           K DISK
001000160308     F*--------
001100160308     FFiar401L  IF   E           K DISK
001200160308     FFnorm01l  IF   E           K DISK
001300970410     F*--------
001400970612     FFNBLT01L  IF   E           K DISK
001500150724     FFNBrv07L  IF   E           K DISK
001600970410     F*--------
001700970410     FFNLBL01L  IF   E           K DISK
001800970410     F*--------
001900970410     FFNLBL02L  IF   E           K DISK
002000970611     F                                     RENAME(FNLBL000:FNLBL2)
002100970611     F*--------
002200130822     FFNBLp32L  IF   E           K DISK
002300130822     F                                     RENAME(FNBLP000:FNBLP32)
002400130822     F*--------
002500130822     FFNBLp33L  IF   E           K DISK
002600130822     F                                     RENAME(FNBLP000:FNBLP33)
002700130822     F*--------
002800970611     FCNACO00F  IF   E           K DISK
002900990610     F*--------
003000040715     F****FNDCT02L  IF   E           K DISK
003100970410     F*--------
003200160407     FTABEL00F  IF   E           K DISK
003300970611     F*--------
003400970611     FEDTAB01L  IF   E           K DISK    USROPN
003500970410     F*--------
003600150724     FAZORG01L  IF   E           k disk
003700970414     F*--------
003800970414     FFNLSI1D   CF   E             WORKSTN
003900150723     f                                     sfile(lsi1S01 : S01nrr)
004000080918     F*--------
004100080918     FTNTBE01L  IF   E           K DISK
004200970410     D*--------------------------------------------------------------*
004300970410     D*  SCHIERE
004400970410     D*--------------------------------------------------------------*
004500970612     D*  Caricamento capoconto clienti
004600970410     D* Schiera codici tabella 3K: invio date consegna
004700030108     D C3K             S              7  0 DIM(9990)
004800030108     D CKS             S              7  0 DIM(9990)
004900030108     D CSD             S              2    DIM(9990)
005000970715     D* Tipi bolla: tabella 3A
005100970715     D C3A             S              2    DIM(500)
005200970715     D TPO             S              2    DIM(500)
005300970701     D* Schiera codici pistole fittizie: 7J
005400970701     D C7J             S              2  0 DIM(500)
005500970611     D* Schiera codici tabella PT
005600150415     D KEYPT           S             35    DIM(500)
005700970611     D KSC             S              7  0 DIM(500)
005800970616     D LNP             S              3  0 DIM(500)
005900970616     D NRS             S              2  0 DIM(500)
006000970616     D LAB             S              1    DIM(500)
006100150415     D* Schiera codici tabella CL
006200150415     D clKSC           S              7  0 DIM(999)
006300150415     D clLNP           S              3  0 DIM(999)
006400980202     D* Schiera codici tabella EM
006500980202     D ELB             S              6    DIM(500)
006600980202     D DLB             S             90    DIM(500)
006700980202     D EVG             S              6    DIM(500)
006800980202     D DVG             S             90    DIM(500)
006900970410     D* Caricamento £1
007000020506     D*UNI             S              3    DIM(29)
007100020506     D £1              S              3  0 DIM(30)
007200970414     D* ERRORI
007300160603     D ERR             S             78    DIM(33) CTDATA PERRCD(1)
007400150723
007500150723     d s01rcd          s                   like(c01rcd)
007600150723     d $Inzs01         s               n   inz(*on)
007700150723     d $Finerec        s               n   inz(*off)
007800150723     d wcev            s                   like(v2ccod)
007900150723     d wsts            s                   like(v2csts)
008000150727     d S01nrr          s              7  0 inz
008100150723     d Datadmy         s               d   datfmt(*dmy)
008200150724     d wtfp            s                   LIKE(BLPTFP)
008300150723     D WrkStringaSql   S           4500
008400150723     D                                     VARYING
008500160307     d ctrvid          s              1    inz('0')
008600160308     d ktrc            s                   like(ar4trc)
008700160407     d kkey            s                   like(tblkey)
008800160603     d werr            s              1
008900970410     D*--------------------------------------------------------------*
009000970410     D* DS
009100970410     D*--------------------------------------------------------------*
009200030924     D Tibs36ds      E DS
009300970612     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
009400970612     D  TCU                  398    697
009500970612     D                                     DIM(50)
009600970612     D  KCU                  698    847P 0
009700970612     D                                     DIM(50)
009800970612     D                                     PACKEVEN
009900970612     D TCUDS           DS
010000970612     D  F3                     3      3
010100970612     D  F4                     4      4
010200970612     D  F56                    5      6
010300970612     D CNCR80        E DS
010400020506     d trul06ds      e ds
010500020506     d  lin                    1     90  0 dim(30)
010600970612     D* DS  appoggio x cmd
010700970616     D CMDAPP          DS            50
010800970616     D  NOMMBR                42     48
010900970617     D CMDLOG          DS            80
011000970617     D  MBRLOG                42     48
011100970617     D  MBRFIS                71     77
011200150724     d fnlv55ds      e ds
011300970414     D WLBDA8          DS
011400970414     D  G02DAT                 1      8  0
011500970414     D  G02INV                 9     16  0
011600970414     D  G02ERR                17     17
011700970414     D  G02TGI                18     22  0
011800150728     d WDAT8           DS
011900150728     d  datada                 1      8  0
012000150728     d  dataa                  9     16  0
012100150728     d  ggl                   17     21  0
012200080421      *----------------------------------------------------------------
012300080421      * Controllo e semaforo x scrittura del VAC: TRUL47R
012400080421      *----------------------------------------------------------------
012500080421     D trul47ds      e ds
012600160317
012700160317     d fior51ds      e ds
012800080421      *
012900970410     D KPJBA         E DS
013000150723     d fnblpds       e ds                  extname(fnblp00f) inz
013100970612     D DSPT          E DS                  EXTNAME(EDIDSPT)
013200970616     D DSPU          E DS                  EXTNAME(EDIDSPU)
013300980202     D DSEM          E DS                  EXTNAME(EDIDSEM)
013400160209     D s_dsem        E DS                  EXTNAME(EDIDSEM)
013500160209     D                                     prefix(S_)
013600150415     D DSCL          E DS                  EXTNAME(EDIDSCL)
013700161104      * ?Parametri pgm interrogaz. Bolle di Filiale FNLRU6R
013800161104     D PARAMu6ds       DS
013900161104     d* CAMPI DI OUTPUT:
014000161104     d* . Key spedizione
014100161104     D  pa1AAS                14     17  0
014200161104     D  pa1LNP                18     20  0
014300161104     D  pa1NRS                21     22  0
014400161104     D  pa1NSP                23     29  0
014500161104     d* . Flag '1'=Premuto F3=Fine
014600161104     D  PA1F03                30     30
014700161104     d* CAMPI DI INPUT:
014800161104     d* . Modalità di richiamo
014900161104     D  PA1FLG                31     31
015000161104     D* . Flag '1'= RICH DA PGM GIAC. (Serve per disabilitare tasto funzionale F
015100161104     d*                                in interrogazione della bolla ed evitare
015200161104     d*                                errore di chiamata ricorsiva
015300161104     D  PA1GIA               144    144
015400161104     d* . Solo dal chiamante FNLRU6R1 per il Perfect Order Wurth:
015500161104     d  pa1rma               149    163
015600161104     d  pa1xco               164    164
015700161104     d* non utilizzato
015800161104     d  pa1flo               165    256
015900161104      * ?Parametri pgm interrogaz. Bolle di Filiale FNLRU6R1
016000161104     D PARAMU6ds1      DS
016100161104     d** INPUT/OUTPUT (se richiesta la selezione bolla)
016200161104     D  pa1AAS_               14     17  0
016300161104     D  pa1LNP_               18     20  0
016400161104     D  pa1NRS_               21     22  0
016500161104     D  pa1NSP_               23     29  0
016600161104     d** OUTPUT
016700161104     D  PA1F03_               30     30
016800161104     d** INPUT
016900161104     D  PA1FLG_               31     31
017000161104     D*    SE RICH DA PGM GIAC. = 1
017100161104     D  PA1GIA_              144    144
017200161104     d** OUTPUT
017300161104     d  pa1rma_              149    163
017400161104     d  pa1xco_              164    164
017500161104     d** INPUT
017600161104     d*   Tipo lancio: "C" -           CHIUSO CON LR
017700161104     d*                "L" - ELABORO E CHIUDO CON LR
017800161104     d*                " " - ELABORO E CHIUDO IN RETRN
017900161104     d  pa1tla_              165    165
018000161104     d  pa1flo_              166    256
018100080421     D***** TIBS55        E DS                  EXTNAME(TIBS55DS)
018200971126     D FNLSI1        E DS                  EXTNAME(FNLSI1DS)
018300970410     D DS3K          E DS
018400970715     D DS3A          E DS
018500970701     D DS7J          E DS
018600970410     D DS7O          E DS
018700150728     D DS3i          E DS
018800060811     D OG143         E DS
018900160308     D dsbl4m        E DS
019000040715
019100040715     d Fidn12ds      e ds
019200040715     d  skKey                 26    305    dim(20)
019300040715     d  skAnn                306    325    dim(20)
019400040715     d  skDca                326    485    dim(20)
019500040715     d  skFca                486    545  0 dim(20)
019600040715     d  skDch                546    705  0 dim(20)
019700040715     d  skCch                706    745    dim(20)
019800040715
019900970410     D                SDS
020000970410     D  NOMPGM                 1     10
020100970410     I*--------------------------------------------------------------*
020200970410     I*  Definizione campi Azorg
020300970410     I*--------------------------------------------------------------*
020400150724     I***AZORG01L  AA
020500150724     I***                                  1    3  ORGDIT
020600150724     I***                             P    4    5 0ORGFIL
020700150724     I***                               3776 3800  ORGDE3
020800160317
020900160317     d fior51r         pr                  extpgm('FIOR51R')
021000160317     d  fior51ds                           likeds(fior51ds)
021100160317
021200150728      /copy gaitrasrc/srcprotopr,xsrlav8
021300970410     C*--------------------------------------------------------------*
021400970410     C*  C I C L O     P R I N C I P A L E
021500970410     C*--------------------------------------------------------------*
021600150723      /free
021700150723         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
021800150723      /end-free
021900970410     C*  Inizializzo varibili videata
022000970410     C                   EXSR      INZ01
022100080421      *
022200970410     C*  Gestione della prima videata
022300970410     C     WFINE         DOWEQ     'N'
022400970611     C     WTPVID        CASEQ     '1'           GESD01
022500970611     C     WTPVID        CASEQ     '2'           GESD02
022600150723     C     WTPVID        CASEQ     'S'           GESsfl
022700970410     C                   END
022800970611     C                   END
022900970410     C*  Fine pgm
023000161114     c                   eval      pa1tla_='C'
023100161114     c                   eval      kpjbu=paramu6ds1
023200161114     c                   call      'FNLRU6R1'
023300161114     c                   parm                    kpjba
023400161114
023500970410     C                   SETON                                        LR
023600970611     C*--------------------------------------------------------------*
023700970611     C* INZ01: Impostazione della prima videata
023800970611     C*--------------------------------------------------------------*
023900970611     C     INZ01         BEGSR
024000970611     C*
024100970611     C                   SETOFF                                       2840
024200970616     C                   MOVEL     '1'           WTPVID
024300970617     C                   MOVEL     'N'           WRIC81            1
024400970616     C                   MOVEL     'N'           WFINE
024500970611     C                   MOVEL     NOMPGM        V1CPGM
024600970611     C                   Z-ADD     0             V1CKSC
024700160408     c                   movel     'E'           v1cass
024800970611     C*
024900970611     C                   ENDSR
025000970410     C*--------------------------------------------------------------*
025100970611     C* INZ02: Impostazione della prima videata
025200970410     C*--------------------------------------------------------------*
025300970611     C     INZ02         BEGSR
025400970410     C*
025500970410     C                   SETOFF                                       284041
025600970611     C                   SETOFF                                       424344
025700970616     C                   MOVEL     '2'           WTPVID
025800970611     C                   MOVEL     NOMPGM        V2CPGM
025900970611     C                   MOVEL     V1CKSC        V2CKSC
026000970611     C                   MOVEL     ACORAG        V2DKSC
026100980204     C                   MOVEL     *BLANKS       V2DCOD
026200980204     C                   MOVEL     *BLANKS       V2CCOD
026300980202     C                   MOVEL     *BLANKS       V2CSTS
026400980202     C                   MOVEL     *BLANKS       V2CFTX
026500970611     C                   MOVE      WHHDAT        V2CAAS
026600130304      **  gestione cliente con Partner gestito da altra filiale
026700130304      **  vedi GWEISS con clienti sulla 277.
026800130304     c                   if        LNP_ALTRA_FIL = 0
026900971007     C                   Z-ADD     LNP(XY)       V2CLNP
027000130821      ******
027100130821     c**** Chiodo x Ducros da Eliminare fra 333 e 330
027200130821     c                   if        v2clnp =333
027300130821     c                   eval      v2clnp =330
027400130821     c                   end
027500130821      ******
027600130304     c                   else
027700130304     C                   eval        V2CLNP = LNP_ALTRA_FIL
027800130304     c                   end
027900971007     C                   Z-ADD     NRS(XY)       V2CNRS
028000970611     C                   Z-ADD     0             V2CNSP
028100970410     C*
028200970410     C                   ENDSR
028300970410     C*--------------------------------------------------------------*
028400970410     C* GESD01: Gestione della prima videata
028500970410     C*--------------------------------------------------------------*
028600970410     C     GESD01        BEGSR
028700970410     C*
028800970410     C* Eseguo exfmt della prima videata
028900970410     C                   EXFMT     LSI1D01
029000970611     C                   SETOFF                                       2840
029100970410     C                   SETOFF                                       4243
029200160407     C                   SETOFF                                       46
029300970410     C* Fine
029400970410     C     *INKC         IFEQ      '1'
029500970414     C                   MOVEL     'S'           WFINE             1
029600970410     C                   GOTO      FINVD1
029700970410     C                   END
029800970410     C* Controlli
029900970410     C                   EXSR      CTR01
030000970410     C   28              GOTO      FINVD1
030100970611     C*
030200970611     C* Se no errori passo a videata successiva
030300150723     c* chiusura bolle per merce mai affidata --> la videata successiva è il sfl
030400150723     c* delle bolle
030500150723     c                   if        v1cfbo='S'
030600150723     C                   MOVEL     'S'           WTPVID
030700150723     c                   eval      $inzS01=*on
030800150723     c                   else
030900150724     C                   EXSR      INZ02
031000150723     C                   MOVEL     '2'           WTPVID
031100150723     c                   endif
031200970410     C*
031300970410     C     FINVD1        ENDSR
031400970611     C*--------------------------------------------------------------*
031500970611     C* GESD02: Gestione della seconda videata
031600970611     C*--------------------------------------------------------------*
031700970611     C     GESD02        BEGSR
031800970611     C*
031900970611     C* Eseguo exfmt della prima videata
032000970611     C                   EXFMT     LSI1D02
032100970611     C                   SETOFF                                       284041
032200970611     C                   SETOFF                                       424344
032300970611     C* Fine
032400970611     C     *INKC         IFEQ      '1'
032500970611     C                   MOVEL     'S'           WFINE             1
032600970617     C* Controllo se devo richiamare TRTC81
032700970617     C     WRIC81        IFEQ      'S'
032800970617     C                   EXSR      CALL81
032900970617     C                   END
033000970611     C                   GOTO      FINVD2
033100970611     C                   END
033200970611     C* Prima videata
033300970611     C     *INKL         IFEQ      '1'
033400970611     C                   MOVEL     '1'           WTPVID            1
033500970617     C* Controllo se devo richiamare TRTC81
033600970617     C     WRIC81        IFEQ      'S'
033700970617     C                   EXSR      CALL81
033800970617     C                   END
033900970611     C                   GOTO      FINVD2
034000970611     C                   END
034100080421      *
034200970612     C* Interrogazione bolle
034300970612     C     *INKG         IFEQ      '1'
034400970616     C     V2CNSP        IFGT      0
034500161104     C                   CLEAR                   paramu6ds1
034600161104     C                   Z-ADD     V2CAAS        pa1AAS_
034700161104     C                   Z-ADD     V2CLNP        Pa1LNP_
034800161104     C                   Z-ADD     V2CNRS        Pa1NRS_
034900161104     C                   Z-ADD     V2CNSP        Pa1NSP_
035000161104     C                   eval      pa1flg_='2'
035100161104     C                   MOVEL     paramu6ds1    KPJBU
035200161104     C                   CALL      'FNLRU6R1'
035300970616     C                   PARM                    KPJBA
035400161104     C                   MOVEL     KPJBU         PARAmu6ds1
035500161104     C     Pa1F03_       IFEQ      *BLANKS
035600161104     C                   Z-ADD     Pa1AAS_       V2CAAS
035700161104     C                   Z-ADD     Pa1LNP_       V2CLNP
035800161104     C                   Z-ADD     Pa1NRS_       V2CNRS
035900161104     C                   Z-ADD     Pa1NSP_       V2CNSP
036000980204     C                   ENDIF
036100970616     C*
036200970616     C                   ELSE
036300970616     C*
036400161104     C                   CLEAR                   PARAmu6ds
036500161104     C                   MOVEL     '2'           Pa1FLG
036600161104     C                   MOVEL     PARAmu6ds     KPJBU
036700161104     C                   CALL      'FNLRU6R'
036800970616     C                   PARM                    KPJBA
036900161104     C                   MOVEL     KPJBU         PARAMu6ds
037000161107     C     Pa1F03        IFEQ      *BLANKS
037100161107     C                   Z-ADD     Pa1AAS        V2CAAS
037200161107     C                   Z-ADD     Pa1LNP        V2CLNP
037300161107     C                   Z-ADD     Pa1NRS        V2CNRS
037400161107     C                   Z-ADD     Pa1NSP        V2CNSP
037500970616     C                   ENDIF
037600980204     C                   ENDIF
037700970612     C                   GOTO      FINVD2
037800970612     C                   END
037900970611     C* Controlli
038000970611     C                   EXSR      CTR02
038100970611     C   28              GOTO      FINVD2
038200970611     C*
038300970611     C* Se no errori vado a pgm successivo
038400970611     C                   EXSR      CALPGM
038500970616     C* Azzero prima videata se ho eseguito conferma
038600970616     C     DI1CMD        IFEQ      '06'
038700970617     C                   EXSR      INZ02
038800970616     C                   END
038900970611     C*
039000970611     C     FINVD2        ENDSR
039100970611     C*--------------------------------------------------------------*
039200970611     C* CTR01: Controlli prima videata
039300970611     C*--------------------------------------------------------------*
039400970611     C     CTR01         BEGSR
039500970611     C*
039600970611     C* Controllo se immesso codice cliente
039700970611     C     V1CKSC        IFEQ      0
039800970611     C                   SETON                                        2840
039900970611     C                   MOVEL     ERR(1)        $MSG
040000970611     C                   END
040100990610     C   28              GOTO      FINCT1
040200150415      *
040300970611     C* Controllo se esiste cliente
040400970612     C                   Z-ADD     1             KKUT
040500970611     C                   Z-ADD     CLIITA        KKCC
040600970611     C                   Z-ADD     V1CKSC        KKSC
040700970611     C     KACO          CHAIN     CNACO00F                           31
040800970611     C     *IN31         IFEQ      '1'
040900150723     c                   clear                   vhdksc
041000970611     C                   SETON                                        2840
041100970611     C                   MOVEL     ERR(2)        $MSG
041200970611     C                   END
041300990610     C   28              GOTO      FINCT1
041400150723     c                   movel     acorag        vhdksc
041500150415      ****
041600970617     C* Controllo Codice cliente unificante tabella 3K
041700030116     C                   Z-ADD     1             XX                5 0
041800970617     C     V1CKSC        LOOKUP    C3K(XX)                                32
041900060811      *
042000060811      **?se non ha agganciato nulla
042100060811     c                   if        *in32 = *off
042200150415      *
042300060811     c     v1cksc        div       10000         trelnp            3 0
042400060811     C                   Z-ADD     treLNP        ORGFIL
042500060811     C     ORGFIL        CHAIN     AZORG01L                           31
042600060811      **? e se  si tratta di DPD (eccezione .......)
042700060811     c                   if        %Found(AZORG01L)
042800060811     c                   movel     orgde3        og143
042900060811     c                   if        §OGNTW ='DPD'
043000060811     C                   Z-ADD     1             XY                5 0
043100150415      ** va direttamente sulla PT poichè DPD non sta sulla 3K
043200060811     C     V1CKSC        LOOKUP    KSC(XY)                                32
043300060811     c                   end
043400060811     c                   end
043500060811      **
043600060811     c                   else
043700060811      **
043800150415      **  con l'unificante della 3K
043900970617     C* Controllo se cliente in tabella PT
044000060811     C                   Z-ADD     1             XY                5 0
044100060811     C     CKS(XX)       LOOKUP    KSC(XY)                                32
044200150415      **
044300060811     c                   end
044400060811      **
044500150415      **  Se non l'avesse trovato con l'UNIFICANTE sulla PT
044600970611     C     *IN32         IFEQ      '0'
044700150415      **
044800970611     C                   SETON                                        2840
044900970611     C                   MOVEL     ERR(3)        $MSG
045000150415      **
045100970617     C                   ELSE
045200020912      *
045300020912      *  la Linea di Partenza del Partner deve essere gestita
045400020912      *   nella £1
045500020912     C     LNP(XY)       lookup    £1                                     32
045600080918      ** ----
045700130304      **     Potrebbe essere un cliente appartenente alla tabella CL
045800130304      **     gestito da un'altra filiale. a questo punto occorre controllare,
045900130304      **     non solo la LINEA del PARTNER agganciata sull'unificante della 3K
046000130304      **     ma se non fosse gestito il Partner sulla filiale si deve vedere
046100130304      **     la Linea del Cliente se è gestita sulla filiale.
046200130304      **   (Esempio su clienti di GWeiss della 360 dati in gestione alla 277.
046300130304      **    il cliente è il 2772159 unificato sulla 3K al 3600001.
046400130304      **   Non è gestita la 360 nella 277 allora il cliente 2772159 è della 277
046500130304      **    quindi deve passare il controllo...poichè il cliente è in gestione)
046600130304     c                   clear                   LNP_ALTRA_FIL     3 0
046700150415      **
046800130304     c                   if        NOT *in32
046900150415      **
047000150415      ** Dovendo gestire casi in cui il cliente è stato spostato ANCORA di
047100150415      **  Filiale mantenendo il codice cliente, occorre effettuare il
047200150415      ** ragionamento sulla LNP presente sulla tab.CL o sulla PT per
047300150415      ** verificare l'appartenenza alla £1 e NON con i primi 3 caratteri del
047400150415      ** codice cliente.
047500150415      ** E'sempre il caso della GWEISS che adesso da 277 viene spostato alla
047600150415      **  249 mantenendo il codice cliente come 277xxxx e facendo capo alla 360
047700150415      **
047800150415     c                   clear                   treLNP            3 0
047900150415     c**** v1cksc        div       10000         treLNP            3 0
048000150415      ** ---
048100150415      **  cerca sulla CL  x impostare la LNP
048200150415     C                   Z-ADD     1             clpt              5 0
048300150415     C     V1CKSC        LOOKUP    clKSC(clpt)                            32
048400150415     c                   if        *in32
048500150415     c                   z-add     clLNP(clpt)   treLNP
048600150415     c                   else
048700150415      **  se non la trova cerca sulla PT  x impostare la LNP
048800150415     C                   Z-ADD     1             clpt              5 0
048900150415     C     V1CKSC        LOOKUP    KSC(clpt)                              32
049000150415     c   32              z-add     LNP(clpt)     treLNP
049100150415     c                   end
049200150415      ** ---
049300150415      **
049400130304     C     treLNP        lookup    £1                                     32
049500130304     c   32              movel     treLNP        LNP_ALTRA_FIL
049600150415      **
049700130304     c                   end
049800130304      ** ----
049900080918      ** Occorre anche controllare se DPD di backup quindi linee collegate
050000080918      **  per potere gestire linee da filiali diverse
050100080918      **   ossia tab."3NN"
050200080918      **
050300080918     C     *IN32         ifeq      *off
050400080918     c                   movel(p)  '3NN'         TbeCOD
050500080918     c                   movel(p)  KSC(xy)       TbeKE1
050600080918     c     kTBE1         setll     tntbe01l
050700080918     c     kTBE1         reade     tntbe01l
050800080918     C                   dow       not %EoF(tntbe01l)
050900080918     c                   movel(p)  tbeKE2        LNPAlternativa    3 0
051000080918     C     LNPAlternativalookup    £1                                     32
051100080918     c                   if        *in32
051200080918     c                   z-add     1             z                 3 0
051300080918     c     *zero         lookup    £1(z)                                  21
051400080918      * aggiunge la linea x il controllo successivo
051500080918     c                   if        *in21
051600080918     C                   z-add     lnp(xy)       £1(z)
051700080918     C                   End
051800080918     C                   Leave
051900080918     C                   End
052000080918     c     kTBE1         reade     tntbe01l
052100080918     C                   End
052200080918     C                   End
052300080918      ** ----
052400020912     C     *IN32         ifeq      *off
052500020912     C                   SETON                                        2840
052600020912     C                   MOVEL     err(8)        $MSG
052700020912     C                   Else
052800020912     C*
052900020912     C                   EndIF
053000020912     C*
053100970617     C                   END
053200020912     C*
053300990610     C   28              GOTO      FINCT1
053400150909     c* Se richiesta visualizzazione bolle da chiudere per merce mai
053500150909     c* affidata ERRORE se non trovato l'evento su tabella EM
053600150909     c                   if        v1cfbo='S' and wsts=*blanks
053700150909     C                   SETON                                        28
053800150909     C                   MOVEL     err(31)       $MSG
053900150909     c                   endif
054000160407     c* Se richiesta visualizzazione bolle da chiudere per merce mai
054100160407     c* il porto deve essere "A" o "F"
054200160407     c                   if        v1cfbo='S' and v1cass=*blanks
054300160407     C                   SETON                                        2846
054400160407     C                   MOVEL     err(32)       $MSG
054500160407     c                   endif
054600080421     C*
054700970611     C     FINCT1        ENDSR
054800150723     C*--------------------------------------------------------------*
054900150723     C* gessfl:  videata sfl per codice cliente
055000150723     C*--------------------------------------------------------------*
055100150723     C     gessfl        BEGSR
055200150723      /free
055300160209         // Valorizzo DSEM con i dati dell'evento di chisura per merce mai aff.
055400160209         dsem=s_dsem ;
055500160209
055600150723         if $inzs01=*on;
055700150723            exsr inzs01;
055800150723            c01csr=1;
055900150723            c01rcd=1;
056000150723            $inzs01=*off;
056100150723         // Se non trovati record errore in prima videata
056200150723            if s01nrr=0   ;
056300150723               $msg=err(27)  ;
056400150723               *in28=*on  ;
056500150723               *in40=*on  ;
056600150723               wtpvid='1'  ;
056700150723               leavesr   ;
056800150723            endif  ;
056900160408       // Le spedizioni in assegnato (orm commissionati) non verranno chiuse
057000160408       // da qui e quindi anche se caricata una sola spedizione non vado mai
057100160408       // alla videata singola ma passo sempre dal sfl
057200160408
057300160408       //   if s01nrr=1;
057400160408       //     chain (vshaas:vshlnp:vshnrs:vshnsp) fnblp01l ;
057500160408       //        kaas=blpaas;
057600160408       //        klnp=blplnp;
057700160408       //        knrs=blpnrs;
057800160408       //        knsp=blpnsp;
057900160408       //     ctrvid=*on ;
058000160408       //     exsr ctr_sfl   ;
058100160408       //     if not *in28  ;
058200160408       //        exsr calpgm;
058300160408       //        chain (vshaas:vshlnp:vshnrs:vshnsp) fnblp01l ;
058400160408       //        if %found(fnblp01l);
058500160408       //           exsr chiuORM;
058600160408       //        endif;
058700160408       //        wtpvid='1' ;
058800160408       //        leavesr;
058900160408       //     endif;
059000160408       //   endif;
059100150723         endif;
059200150723       // c01csr   contiene il numero di riga del subfile su cui era posizionato
059300150723       // impostando c01rcd   visualizzo la pagina che vedeva l'utente quando ha
059400150723       // l'ultimo tasto
0595001507232         if c01csr   > 0;
059600150723           c01rcd   = c01csr;
059700150723          else;
059800150723           c01rcd   = s01rcd   ;
0599001507232         endif;
060000150723       // se non so quale pagina visualizzare forzo pagina 1
0601001507232         if c01rcd   < 1;
060200150723           eval c01rcd   = 1;
0603001507232         endif;
060400150723
060500150723       // valorizzo campi del sfl control
060600150723          vc1ksc=%int(v1cksc);
060700150724          vc1kscd=vhdksc;
060800160408          if v1cass='I' ;
060900160408             vc1por='ORM Commissionati';
061000160407          else;
061100160408             clear vc1por              ;
061200160407          endif;
061300150723
061400150723         // Emissione videata
061500150723         write  lsi1z01;
061600150723         exfmt  lsi1C01;
061700150723
061800150723         s01nrr=1  ;
061900150723
062000150723 1       SELECT;
062100150723
062200150723         // - F3=Fine
062300150723 1         WHEN  *inkc;
062400150723               wFine = 'S';
062500150723
062600150723         // - F12=Ritorno
062700150723 1         WHEN  *inkl;
062800150914               *in28=*off;
062900150723               wtpvid='1'  ;
063000150723
063100150723
063200150723         // -
063300160603 1         WHEN  *inkb ;
063400160603             exsr seltot;
063500160603             wtpvid='1';
063600150723
063700150723
063800150723 x1      // Invio
063900150723           OTHER;
064000150723
064100150723               exsr ContrS01 ;
064200150723
064300150723 1       ENDSL;
064400150723      /end-free
064500150723     c                   endsr
064600150723      /free
064700150723       //--------------------------------------------------------------
064800150723       BEGSR InzS01;
064900150723
065000150723       // Pulizia subfile
065100150723         *in30  = *on;
065200150723         *in31  = *on;
065300150723         write  lsi1c01;
065400150723         *in31  = *off;
065500150723         *in30  = *off;
065600150723         *in32  = *off;
065700150723
065800150723         exec sql close BLPcsr;
065900150723       // Richiamo routine di preparazione stringa SQL
066000150723             exsr sr_prepsql;
066100150723
066200150723         S01nrr=0 ;
066300150723         clear $msg;
066400150723         *in28  = *off;
066500150723         *in90  = *off;
066600150728         *in19=*off;
066700160308         *in45=*off;
066800150723       // Elaboro i records estratti
066900150723           $finerec=*off;
067000150723           exec sql prepare STRINGASQL from :WrkStringaSql;
067100150723           exec sql declare BLPCSR cursor for StringaSql;
067200150723           exec sql open BLPcsr;
067300150723           dow $finerec=*off;
067400150723              exec sql Fetch BLPcsr into :fnblpds;
067500150728              *in28  = *off;
067600150728              if sqlcod=100 or sqlcod<0;
067700150723                 $finerec = *on;
067800150723                 leave;
067900150723              endif;
068000150728              if s01nrr=9999;
068100150728                 $finerec = *on;
068200150728                 $msg=err(30);
068300150728                 *in28=*on;
068400150728                 leave;
068500150728              endif;
068600150723              if %lookup(blplnp:£1)>0;
068700150724                 kaas=blpaas;
068800150724                 klnp=blplnp;
068900150724                 knrs=blpnrs;
069000150724                 knsp=blpnsp;
069100160405       // Vedo se assegnato o franco
069200160407                 kkey=blpcbo;
069300160407                 clear ds3a;
069400160407                 chain (1:'3A':kkey) tabel00f;
069500160407                 if %found(tabel00f);
069600160407                    ds3a=tbluni;
069700160407                 endif;
069800160407       // escludo in base al porto richiesto
069900160408                 if (v1cass='I' and %subst(§3atb1:1:1)='A')  or
070000160408                    (v1cass='E' and %subst(§3atb1:1:1)<>'A')  ;
070100150728       //per poter chiudere la spedizione come merce mai affidata
070200150728       //devono essere trascorsi i giorni max di sospensione previsti
070300150728       //da tabella
070400160405       //I giorni variano a seconda che si tratti di assegnato oppure franco
070500160405
070600150728                 clear wdat8;
070700150728                 datada=blpdim;
070800150728                 dataa=(blpaas*10000)+blpmgs;
070900150728                 xsrlav8(wdat8);
071000160407                 if (%subst(§3atb1:1:1)<>'A' and (ggl-1) >  §3iggc) or
071100170605                    (%subst(§3atb1:1:1)='A' and (ggl-1) >  §3iggc) ;
071200170605          //        (%subst(§3atb1:1:1)='A' and (ggl-1) >  §3igga) ;
071300160307                    ctrvid=*off;
071400150728                    exsr ctr_sfl;
071500150728                    if not *in28;
071600150728           // Carico record nel sfl
071700150728                       exsr carsf01;
071800150728                    endif;
071900150728                 endif;
072000160405               endif;
072100150723              endif;
072200150723           enddo;
072300150723
072400150723          s01rcd    = s01nrr  ;
072500150723
072600150723         ENDSR  ;
072700150723       //--------------------------------------------------------------
072800150723       //?Preparazione stringa SQL
072900150723       //--------------------------------------------------------------
073000150723       BEGSR sr_prepsql;
073100150723       WrkStringaSql='select * from fnblp00f' +
073200150724       ' where (blpksc=' + %editc(v1cksc:'X') + ' or '      +
073300150724       ' blpccm=' + %editc(v1cksc:'X') +
073400150724       ') and blptfp=' + %editc(wtfp:'X') +
073500150723       ' and blpnfv=0' +
073600150723       ' order by  blptfp, blpnfv, blpdim';
073700150723       endsr;
073800150723       //--------------------------------------------------------------
073900150723       //?eseguo caricamento SFL
074000150723       //--------------------------------------------------------------
074100150723          BEGSR   carSF01    ;
074200150723          clear vs1opz  ;
074300150723
074400150723          exsr CarRECORD  ;
074500150723          vshaas=blpaas;
074600150723          vshlnp=blplnp;
074700150723          vshnrs=blpNRs;
074800150723          vshnsp=blpNsp;
074900150723          s01nrr=s01nrr+1   ;
075000150723          write  lsi1S01;
075100150723
075200150723          ENDSR         ;
075300150723       //--------------------------------------------------------------
075400150723       //?Caricamento record SFL
075500150723       //--------------------------------------------------------------
075600150723       BEGSR CarRecord;
075700150723       // data Immissione bolla
075800150723             clear vs1dim;
075900150723             datadmy=%date(blpdim:*iso);
076000150723             vs1dim=%dec(datadmy);
076100150723       // Data Spedizione
076200150723             clear vs1dsp;
076300150723             eval datadmy=%date((blpaas*10000+blpmgs):*iso);
076400150723             vs1dsp=%dec(datadmy);
076500150723       // linea di arrivo
076600150723             vs1lna=blplna;
076700150723             chain blplna azorg01l ;
076800150723             if orgde5<>*blanks;
076900150723                 vs1lnad=%subst(orgde5:1:10);
077000150723             else;
077100150723                 vs1lnad=%subst(orgdes:1:10);
077200150723             endif;
077300150723       // Destinatario
077400150723             vs1rsd=blprsd;
077500160317       // Visualizzo se assegnato
077600160407             clear vs1ass;
077700160408               if v1cass='I';
077800160408                  vs1ass=§3atb1;
077900160408               endif;
078000150723       ENDSR         ;
078100150723       //--------------------------------------------------------------
078200150723       //?controlli SFL
078300150723       //--------------------------------------------------------------
078400150723       BEGSR ContrS01;
078500150723
078600160308       *in32=*off ;
078700150723       s01nrr=1  ;
078800150723       readc lsi1s01    ;
078900150723
079000150723 0     dow not %eof                      ;
079100150723
079200160308            *in45=*off;
079300160308            *in28=*off;
079400150723       // Verifica l'opzione
079500150723          if vs1opz<>*blanks    ;
079600150723            *in19=*off;
079700150723          select;
079800150723          when vs1opz='1'                     ;
079900150724            chain (vshaas:vshlnp:vshnrs:vshnsp) fnblp01l ;
080000150914                 kaas=blpaas;
080100150914                 klnp=blplnp;
080200150914                 knrs=blpnrs;
080300150914                 knsp=blpnsp;
080400160308            ctrvid=*on ;
080500160308            exsr ctr_sfl;
080600150724            if *in28;
080700160308       //      eval *in19=*on;
080800160308               eval *in45=*on;
080900160308               eval *in32=*on;
081000150724            else;
081100160308               exsr calpgm;
081200150724            endif;
081300150724            chain (vshaas:vshlnp:vshnrs:vshnsp) fnblp01l ;
081400160317       // Se spedizione in assegnato richiamo fior51r per eventuale chiusura
081500160317       // dell'ORM commissionato
081600160317            if %found(fnblp01l);
081700160330       //      if vs1ass<>*blanks;
081800160330       //         chain (blpaas:blplnp:blpnrs:blpnsp:'M') fiar401l;
081900160330       //         if %found(fiar401l);
082000160330       //           dsbl4m=ar4not;
082100160330       //           clear fior51ds;
082200160330       //           I51POE=§B4POE;
082300160330       //           I51NSR=§B4NSR;
082400160330       //           I51NOR=§B4NOR;
082500160330       //           I51NRV=§B4NRV;
082600160330       //           I51FAR=999;
082700160330       //           fior51r(fior51ds);
082800160330       //         endif;
082900160330       //      endif;
083000160330               exsr chiuORM;
083100150723            if blpdcm>0;
083200150723               eval *in19=*on;
083300150723            endif;
083400160317            endif;
083500150723          when vs1opz='5';
083600150723               exsr int_bolla;
083700150723          endsl;
083800150723               clear vs1opz;
083900150723          endif;
084000160308               update  lsi1s01  ;
084100160308
084200160308          *in32=*off ;
084300150723
084400150723       readc    lsi1s01    ;
084500150723 0     enddo         ;
084600150723
084700150723       ENDSR  ;
084800160603       //--------------------------------------------------------------
084900160603       //?Chisura di tutti i record caricati nel SFL
085000160603       //--------------------------------------------------------------
085100160603       BEGSR Seltot  ;
085200160603
085300160603       clear werr;
085400160603       s01nrr=1  ;
085500160603       chain s01nrr lsi1s01    ;
085600160603
085700160603 0     dow %found                        ;
085800160603
085900160603            *in28=*off;
086000160603       // Verifica l'opzione
086100160603            chain (vshaas:vshlnp:vshnrs:vshnsp) fnblp01l ;
086200160603                 kaas=blpaas;
086300160603                 klnp=blplnp;
086400160603                 knrs=blpnrs;
086500160603                 knsp=blpnsp;
086600160603                 exsr ctr_sfl;
086700160603            if not *in28;
086800160603               exsr calpgm;
086900160603            endif;
087000160603            chain (vshaas:vshlnp:vshnrs:vshnsp) fnblp01l ;
087100160603       // Se spedizione in assegnato richiamo fior51r per eventuale chiusura
087200160603       // dell'ORM commissionato
087300160603            if %found(fnblp01l);
087400160603               exsr chiuORM;
087500160603               if blpdcm=0;
087600160603                  werr='1';
087700160603               endif;
087800160603            endif;
087900160603       s01nrr+=1;
088000160603       chain s01nrr lsi1s01    ;
088100160603 0     enddo         ;
088200160603       // Terminata l'elaborazione di tutti i record di sfl, se presenti errori
088300160603       // emetto messaggio in prima videata
088400160603       if werr='1';
088500160603               $msg=err(33)  ;
088600160603               *in28=*on  ;
088700160603       else     ;
088800160603               *in28 = *off;
088900160603               *in40 = *off;
089000160603               $msg=*blANKS;
089100160603       endif;
089200160603
089300160603       ENDSR  ;
089400150723       //--------------------------------------------------------------
089500150723       //?Aggiornamento sfl
089600150723       //--------------------------------------------------------------
089700150723       BEGSR AGGIOSFL  ;
089800150723
089900150723       //Sflnxtchg=*on  ;
090000150723
090100150723       update  lsi1s01  ;
090200150723
090300150723       ENDSR   ;
090400160330       //--------------------------------------------------------------
090500160330       //?Chiusura ORM
090600160330       //--------------------------------------------------------------
090700160330       BEGSR ChiuORM   ;
090800160330               if vs1ass<>*blanks;
090900160330                  chain (blpaas:blplnp:blpnrs:blpnsp:'M') fiar401l;
091000160330                  if %found(fiar401l);
091100160330                    dsbl4m=ar4not;
091200160330                    clear fior51ds;
091300160330                    I51POE=§B4POE;
091400160330                    I51NSR=§B4NSR;
091500160330                    I51NOR=§B4NOR;
091600160330                    I51NRV=§B4NRV;
091700160330                    I51FAR=999;
091800160330                    fior51r(fior51ds);
091900160330                  endif;
092000160330               endif;
092100160330       ENDSR   ;
092200150723      /end-free
092300150723     C*--------------------------------------------------------------*
092400150723     C* interroga bolla in partenza
092500150723     C*--------------------------------------------------------------*
092600150723     C     int_bolla     BEGSR
092700161107     c                   clear                   paramu6ds1
092800161107     C                   Z-ADD     VshAAS        Pa1AAS_
092900161107     C                   Z-ADD     VshLNP        Pa1LNP_
093000161107     C                   Z-ADD     VshNRS        Pa1NRS_
093100161107     C                   Z-ADD     VshNSP        Pa1NSP_
093200161107     C                   MOVEL     paramu6ds1    KPJBU
093300161107     C                   CALL      'FNLRU6R1'
093400161107     C                   PARM                    KPJBA
093500150723     c                   endsr
093600970410     C*--------------------------------------------------------------*
093700130822     C* Decodifica i 2 riferimenti per impostare la spedizione
093800970410     C*--------------------------------------------------------------*
093900130822     C     decod_da_Rif  BEGSR
094000970410     C*
094100131016     c                   clear                   piu_bolle_Xrif    1
094200131016     c                   clear                   V2NOTE1
094300131016     c                   clear                   V2NOTE2
094400131016     C*
094500130822     c                   clear                   se_trovato        1
094600130822     c                   eval      se_trovato ='N'
094700130822     C*
094800130822     c                   if        v2rif32 <> *blank
094900131016     c     v2rif32       setll     fnblp32l
095000131016     c     v2rif32       reade     fnblp32l
095100131016     c                   dow       not %EoF(fnblp32l)
095200130822     c                   eval      se_trovato ='S'
095300131016     c     v2rif32       reade     fnblp32l
095400131016     c                   if        not %EoF(fnblp32l)
095500131016     c                   eval       piu_bolle_Xrif ='S'
095600131016     c                   end
095700130822     c                   end
095800130822     c                   end
095900130822      *
096000130822     c                   if        v2rif33 > 0 and
096100130822     c                             se_trovato ='N'
096200131016     c     v2rif33       setll     fnblp33l
096300131016     c     v2rif33       reade     fnblp33l
096400131016     c                   dow       not %Eof(fnblp33l)
096500130822     c                   eval      se_trovato ='S'
096600131016     c     v2rif33       reade     fnblp33l
096700131016     c                   if        not %EoF(fnblp33l)
096800131016     c                   eval       piu_bolle_Xrif ='S'
096900131016     c                   end
097000130822     c                   end
097100130822     c                   end
097200130822     C*
097300130822     C* Se lo ha trovato imposta imposta i campi della spedizione
097400131016     C*   ATTENZIONE potrebbero essercene più di una con lo stesso
097500131016     C* riferimento quindi segnala che ha preso l'ultima
097600130822     c                   IF        se_trovato ='S'
097700131016     c                               and v2cnsp = 0
097800130822     c                   eval      v2caas = blpaas
097900130822     c                   eval      v2clnp = blplnp
098000130822     c                   eval      v2cnrs = blpnrs
098100130822     c                   eval      v2cnsp = blpnsp
098200130822     c                   end
098300131016     C*   AVVISA SE HA trovato più bolle con lo stesso riferimento
098400131016     c                   IF         piu_bolle_Xrif ='S'
098500131016     c                   EVAL        V2NOTE1 = err(25)
098600131016     c                   EVAL        V2NOTE2 = err(26)
098700131016     c                   end
098800131016     C*
098900130822     C                   ENDSR
099000130822     C*--------------------------------------------------------------*
099100130822     C* CTR02: Controlli prima videata
099200130822     C*--------------------------------------------------------------*
099300130822     C     CTR02         BEGSR
099400130822     C*
099500130822     C*  pre- controllo bolla tramite rif.alfa o numerico
099600130822     c                   exsr      decod_da_Rif
099700130822     C*
099800970410     C* Controllo se immesso anno
099900970611     C     V2CAAS        IFEQ      0
100000970410     C                   SETON                                        2840
100100970611     C                   MOVEL     ERR(5)        $MSG
100200970410     C                   END
100300990610     C   28              GOTO      FINCT2
100400970410     C* Se anno digitato di 2 --> 4 cifre
100500970612     C                   MOVEL     V2CAAS        WSECOL            2 0
100600970410     C     WSECOL        IFEQ      0
100700970611     C     V2CAAS        IFGT      60
100800970611     C                   ADD       1900          V2CAAS
100900970410     C                   ELSE
101000970611     C                   ADD       2000          V2CAAS
101100970410     C                   END
101200970410     C                   END
101300970410     C* Controllo se immessa linea di partenza
101400970611     C     V2CLNP        IFEQ      0
101500970414     C                   SETON                                        2841
101600970611     C                   MOVEL     ERR(6)        $MSG
101700970410     C                   END
101800990610     C   28              GOTO      FINCT2
101900970410     C* Controllo esistenza lnp
102000970611     C                   Z-ADD     V2CLNP        ORGFIL
102100970410     C     ORGFIL        CHAIN     AZORG01L                           31
102200970611     C     *IN31         IFEQ      '1'
102300970410     C                   SETON                                        2841
102400970611     C                   MOVEL     ERR(7)        $MSG
102500970414     C                   END
102600990610     C   28              GOTO      FINCT2
102700970414     C* Controllo se lnp in £1
102800970611     C     V2CLNP        LOOKUP    £1                                     32
102900970414     C     *IN32         IFEQ      '0'
103000970414     C                   SETON                                        2841
103100970611     C                   MOVEL     ERR(8)        $MSG
103200970414     C                   END
103300990610     C   28              GOTO      FINCT2
103400970414     C* Controllo se immesso numero di spedizione
103500970611     C     V2CNSP        IFEQ      0
103600970414     C                   SETON                                        2843
103700970611     C                   MOVEL     ERR(9)        $MSG
103800970414     C                   END
103900990610     C   28              GOTO      FINCT2
104000970414     C* Controllo se esiste spedizione
104100970611     C                   Z-ADD     V2CAAS        KAAS
104200970611     C                   Z-ADD     V2CLNP        KLNP
104300970611     C                   Z-ADD     V2CNRS        KNRS
104400970611     C                   Z-ADD     V2CNSP        KNSP
104500970414     C     KBLP          CHAIN(N)  FNBLP01L                           31
104600970414     C     *IN31         IFEQ      '1'
104700970414     C                   SETON                                        2843
104800970611     C                   MOVEL     ERR(10)       $MSG
104900970616     C                   ELSE
105000970616     C* Controllo che la spedizione sia del cliente selezionato
105100970616     C     BLPCCM        IFNE      V2CKSC
105200970616     C     BLPKSC        ANDNE     V2CKSC
105300970616     C                   SETON                                        2843
105400980202     C                   MOVEL     ERR(17)       $MSG
105500970414     C                   END
105600970616     C                   END
105700990610     C   28              GOTO      FINCT2
105800970414     C* Controllo se bolla mamma
105900150728     C*                  Z-ADD     V2CAAS        KAAP
106000150728     C*                  Z-ADD     V2CLNP        KLNP
106100150728     C*                  Z-ADD     V2CNRS        KNRP
106200150728     C*                  Z-ADD     V2CNSP        KNSP
106300150728     C*    KBLP          CHAIN     FNLBL02L                           31
106400970414     C* Controllo se bolla figlia
106500150728     C*                  Z-ADD     V2CAAS        KAAP
106600150728     C*                  Z-ADD     V2CLNP        KLNP
106700150728     C*                  Z-ADD     V2CNRS        KNRP
106800150728     C*                  Z-ADD     V2CNSP        KNSP
106900150728     C*    KBLP          CHAIN     FNLBL01L                           31
107000150728     C*    *IN31         IFEQ      '0'
107100150728     C*                  SETON                                        284344
107200150728     C*                  MOVEL     ERR(12)       $MSG
107300150728     C*                  END
107400150728     C*  28              GOTO      FINCT2
107500980204     C* Controllo che non sia stata data per consegnata
107600150728     C*    BLPDCM        IFNE      0
107700150728     C*    BLPCCA        ANDNE     '6'
107800150728     C*    BLPDCM        ORNE      0
107900150728     C*    BLPCCA        ANDNE     '2'
108000150728     C*                  SETON                                        2843
108100150728     C*                  MOVEL     ERR(22)       $MSG
108200150728     C*                  END
108300150728     C*  28              GOTO      FINCT2
108400980204     C* Controllo che abbia il dettaglio segnacolli
108500150728     C*                  Z-ADD     V2CAAS        KAAS
108600150728     C*+                 Z-ADD     V2CLNP        KLNP
108700150728     C**                 Z-ADD     V2CNRS        KNRS
108800150728     C*                  Z-ADD     V2CNSP        KNSP
108900150728     C*    KBLT          CHAIN     FNBLT01L                           31
109000150728     C*    *IN31         IFEQ      '1'
109100150728     C*                  SETON                                        2843
109200150728     C*                  MOVEL     ERR(16)       $MSG
109300150728     C*                  END
109400150728     c                   exsr      ctr_uniS
109500150728     C   28              GOTO      FINCT2
109600980202     C* Controllo se richiesto '?'
109700980202     C     '?'           SCAN      V2CSTS                                 32
109800980202     C     *IN32         IFEQ      '1'
109900980202     C                   MOVE      LAB(XY)       DI1LAB
110000980202     C                   MOVEL     FNLSI1        KPJBU
110100980203     C                   CALL      'FNLSI1R5'
110200980202     C                   PARM                    KPJBA
110300980204     C                   MOVEL     KPJBU         FNLSI1
110400980204     C                   MOVEL     DI1CEV        V2CCOD
110500980202     C                   MOVEL     DI1STS        V2CSTS
110600980202     C                   END
110700980202     C* evento obbligatoria
110800980202     C     V2CCOD        IFEQ      *BLANKS
110900980202     C     V2CSTS        ANDEQ     *BLANKS
111000970721     C                   SETON                                        2844
111100980202     C                   MOVEL     ERR(21)       $MSG
111200970721     C                   END
111300990610     C   28              GOTO      FINCT2
111400980202     C* evento esistente
111500980202     C                   MOVEL     V2CSTS        WEVE              6
111600980202     C                   MOVE      V2CCOD        WEVE
111700030116     C                   Z-ADD     1             YY                5 0
111800980202     C     LAB(XY)       IFEQ      'S'
111900980202     C     WEVE          LOOKUP    ELB(YY)                                32
112000980202     C                   MOVEL     DLB(YY)       DSEM
112100980202     C                   ELSE
112200980202     C     WEVE          LOOKUP    EVG(YY)                                32
112300980202     C                   MOVEL     DVG(YY)       DSEM
112400980203     C                   MOVEL     §EMDES        V2DCOD
112500980202     C                   END
112600980204     C     *IN32         IFEQ      '0'
112700980202     C                   SETON                                        2844
112800980202     C                   MOVEL     ERR(19)       $MSG
112900980202     C                   END
113000990610     C   28              GOTO      FINCT2
113100980203     C* Se bolla in assegnato no IDD Totale definitivo
113200150728     C*                  Z-ADD     1             X
113300150728     C*    BLPCBO        LOOKUP    C3A(X)                                 32
113400150728     C*                  MOVEL     TPO(X)        WTPO              1
113500150728     C*    *IN32         IFEQ      '1'
113600150728     C*    WTPO          ANDEQ     'A'
113700150728     C*    §EMASS        ANDNE     'S'
113800160405     C*                  SETON                                        2843
113900150728     C*                  MOVEL     ERR(20)       $MSG
114000150728     C*                  END
114100150728     C*  28              GOTO      FINCT2
114200970414     C* Controllo che non sia stata data per partita
114300150728     C*    BLPDT1        IFNE      0
114400150728     C*    §EMTRA        ANDNE     'S'
114500150728     C*                  SETON                                        2843
114600150728     C*                  MOVEL     ERR(13)       $MSG
114700150728     C*                  END
114800150728     C*  28              GOTO      FINCT2
114900970414     C* Controllo che non sia stata trasmessa in sede
115000150728     C*    BLPDT2        IFNE      0
115100150728     C*    §EMTRA        ANDNE     'S'
115200150728     C*                  SETON                                        2843
115300150728     C*                  MOVEL     ERR(14)       $MSG
115400150728     C*                  END
115500150728     C*  28              GOTO      FINCT2
115600970414     C* Controllo che non sia stata trasmessa al cliente
115700150728     C*    BLPDT3        IFNE      0
115800150728     C*    §EMTRA        ANDNE     'S'
115900150728     C*                  SETON                                        2843
116000150728     C*                  MOVEL     ERR(15)       $MSG
116100150728     C*                  END
116200150728     C*  28              GOTO      FINCT2
116300990610      * Controllo C.A.
116400150728     C*    §EMCAC        IFEQ      'S'
116500040715      *
116600150728     c*                  clear                   fidn12ds
116700150728     c*                  eval      i12aas = V2CAAS
116800150728     c*                  eval      i12lnp = V2CLNP
116900150728     c*                  eval      i12nrs = V2CNRS
117000150728     c*                  eval      i12nsp = V2CNSP
117100150728     c*                  eval      i12fel = 'E'
117200150728     c*                  eval      i12fan = 'E'
117300150728     c*                  eval      i12fch = 'E'
117400150728     c*                  call      'FIDN12R'
117500150728     c*                  parm                    fidn12ds
117600040715      **
117700040715      * se c'è almeno una C.A. aperta
117800150728     c*    o12nca        ifgt      0
117900150728     C*                  SETON                                        284331
118000150728     C*                  MOVEL     ERR(24)       $MSG
118100150728     c*                  end
118200040715      **
118300150728     C*                  ENDIF
118400040715      **
118500160307     c                   eval      ctrvid=*on
118600150728     c                   exsr      ctr_uniE
118700990610     C   28              GOTO      FINCT2
118800040715      **
118900980204     C* Controllo se l'evento è già stato inviato
119000980204     C                   Z-ADD     V2CAAS        KAA2
119100980204     C                   MOVEL     §EMCOD        KCEV
119200980204     C                   MOVEL     ' '           WDAFOR            1
119300990810     C     KEVB          CHAIN     FNEVB04L                           31
119400980204     C* Evento già immesso
119500980204     C     *IN31         IFEQ      '0'
119600980204     C* se non è possibile forzare do errore
119700980204     C     §EMFOR        IFNE      'S'
119800980204     C                   SETON                                        2843
119900980204     C                   MOVEL     ERR(23)       $MSG
120000980204     C                   ELSE
120100980204     C                   MOVEL     'F'           WDAFOR
120200980204     C                   END
120300980204     C                   END
120400990610     C   28              GOTO      FINCT2
120500970414     C*
120600970612     C     FINCT2        ENDSR
120700150724     C*--------------------------------------------------------------*
120800150728     C*    Controlli per il caricamento e gestione sfl
120900150724     C*--------------------------------------------------------------*
121000150728     C     ctr_sfl       BEGSR
121100150724     c                   clear                   $msg
121200150724     c                   setoff                                       28
121300150728     c                   exsr      ctr_uniS
121400150728     c                   if        *in28
121500150728     c                   leavesr
121600150728     c                   endif
121700150728     c                   exsr      ctr_uniE
121800150728     c                   if        *in28
121900150728     c                   leavesr
122000150728     c                   endif
122100150724     c                   clear                   wsispu            1
122200150724     c* controllo che la spedizione non abbia spunte
122300150728     c     kblt          setll     fnblt01l
122400150728     c     kblt          reade     fnblt01l                               99
122500150724    2c                   dow       not *in99
122600150724     c
122700150724     c* Cerco una qualsiasi spunta
122800150724     c     kbrv          chain     fnbrv07l
122900150724    3c                   if        %found(fnbrv07l)
123000150724     c                   eval      wsispu='S'
123100150724     c                   seton                                        99
123200150724   x3c                   else
123300150728     c     kblt          reade     fnblt01l                               99
123400150724    3c                   endif
123500150724    2c                   enddo
123600150724     c*
123700150724    2c                   if        wsispu='S'
123800150724     c                   seton                                        28
123900150724     C                   MOVEL     ERR(29)       $MSG
124000150724     c                   leavesr
124100150724     c                   endif
124200150724     c
124300150724     c
124400150724     c                   endsr
124500150728     C*--------------------------------------------------------------*
124600150728     C*    Controlli unici per spedizione
124700150728     C*--------------------------------------------------------------*
124800150728     C     ctr_uniS      BEGSR
124900150728     C* Controllo se bolla figlia
125000150728     C     KBLP          CHAIN     FNLBL01L
125100150728     C                   if        %found(fnlbl01l)
125200150728     C                   SETON                                        28
125300150728     C                   SETON                                          4344
125400150728     C                   MOVEL     ERR(12)       $MSG
125500150728     c                   leavesr
125600150728     C                   END
125700150728     C* Controllo che non sia stata data per consegnata
125800150728     C     BLPDCM        IFNE      0
125900150728     C     BLPCCA        ANDNE     '6'
126000150728     C     BLPDCM        ORNE      0
126100150728     C     BLPCCA        ANDNE     '2'
126200150728     C                   SETON                                        28
126300150728     C                   SETON                                        43
126400150728     C                   MOVEL     ERR(22)       $MSG
126500150728     c                   leavesr
126600150728     C                   END
126700150728     C* Controllo che abbia il dettaglio segnacolli
126800150728     c     kblt          setll     fnblt01l
126900150728     C                   IF        not %equal(fnblt01l)
127000150728     C                   SETON                                        28
127100150728     C                   SETON                                        43
127200150728     C                   MOVEL     ERR(16)       $MSG
127300150728     c                   leavesr
127400150728     C                   END
127500150728     c                   endsr
127600150728     C*--------------------------------------------------------------*
127700150728     C*    Controlli unici legati all'evento
127800150728     C*--------------------------------------------------------------*
127900150728     C     ctr_uniE      BEGSR
128000150728     C* Se bolla in assegnato no IDD
128100160307     c* Solo se vengo da controlli videata
128200160317     c*                  if        ctrvid=*on
128300160317     C*                  Z-ADD     1             X
128400160317     C*    BLPCBO        LOOKUP    C3A(X)                                 32
128500160317     C*                  MOVEL     TPO(X)        WTPO              1
128600160317     C*    *IN32         IFEQ      '1'
128700160317     C*    WTPO          ANDEQ     'A'
128800160317     C*    §EMASS        ANDNE     'S'
128900160317     c*                  eval      ktrc='M'
129000160317     c*    kar4          chain     fiar401l
129100160317     c*                  if        %found(fiar401l)
129200160317     c*                  eval      dsbl4m=ar4not
129300160317     c*    korm          chain     fnorm01l
129400160317     c*                  if        %found(fnorm01l) and ormfao<>999
129500160317     C*                  SETON                                        28
129600160317     C*                  SETON                                        43
129700160317     C*                  MOVEL     ERR(20)       $MSG
129800160317     c*                  leavesr
129900160317     c*                  endif
130000160317     c*                  endif
130100160317     C*                  END
130200160317     c*                  endif
130300150728     C* Controllo che non sia stata data per partita
130400150728     C     BLPDT1        IFNE      0
130500150728     C     §EMTRA        ANDNE     'S'
130600150728     C                   SETON                                        2843
130700150728     C                   MOVEL     ERR(13)       $MSG
130800150728     c                   leavesr
130900150728     C                   END
131000150728     C* Controllo che non sia stata trasmessa in sede
131100150728     C     BLPDT2        IFNE      0
131200150728     C     §EMTRA        ANDNE     'S'
131300150728     C                   SETON                                        2843
131400150728     C                   MOVEL     ERR(14)       $MSG
131500150728     c                   leavesr
131600150728     C                   END
131700150728     C* Controllo che non sia stata trasmessa al cliente
131800150728     C     BLPDT3        IFNE      0
131900150728     C     §EMTRA        ANDNE     'S'
132000150728     C                   SETON                                        2843
132100150728     C                   MOVEL     ERR(15)       $MSG
132200150728     c                   leavesr
132300150728     C                   END
132400150728      * Controllo C.A.
132500150728      *
132600150728     C     §EMCAC        IFEQ      'S'
132700150728     c                   clear                   fidn12ds
132800150728     c                   eval      i12aas = blpaAS
132900150728     c                   eval      i12lnp = blpLNP
133000150728     c                   eval      i12nrs = blpNRS
133100150728     c                   eval      i12nsp = blpNSP
133200150728     c                   eval      i12fel = 'E'
133300150728     c                   eval      i12fan = 'E'
133400150728     c                   eval      i12fch = 'E'
133500150728     c                   call      'FIDN12R'
133600150728     c                   parm                    fidn12ds
133700150728      **
133800150728      * se c'è almeno una C.A. aperta
133900150728     c     o12nca        ifgt      0
134000150728     C                   SETON                                        28
134100150728     C                   SETON                                          4331
134200150728     C                   MOVEL     ERR(24)       $MSG
134300150728     c                   leavesr
134400150728     c                   end
134500150728     C                   ENDIF
134600150728     c                   endsr
134700970612     C*--------------------------------------------------------------*
134800970612     C* CALPGM: Eseguo Call a pgm
134900970612     C*--------------------------------------------------------------*
135000970612     C     CALPGM        BEGSR
135100970612     C*
135200970612     C*  Imposto DS per richiamo altri pgm
135300970612     C                   CLEAR                   FNLSI1
135400150723     c                   if        wtpvid='S'
135500150723     C                   clear                   DI1FTX
135600150723     C                   MOVEL     V1CKSC        DI1KSC
135700150723     C                   MOVEL     VhdKSC        DI1DEC
135800150723     C                   MOVEL     VshLNP        DI1LNP
135900150723     C                   MOVEL     VshNRS        DI1NRS
136000150723     C                   MOVEL     VshNSP        DI1NSP
136100150723     C                   MOVEL     VshAAS        DI1AAS
136200150723     C                   MOVEL     VshAAS        DI1DSP
136300150723     C                   MOVE      BLPMGS        DI1DSP
136400150723     C                   MOVE      LAB(XY)       DI1LAB
136500150723     C                   MOVE      wsts          DI1STS
136600150723     C                   MOVE      wcev          DI1CEV
136700160209     C**                 MOVEL     wsts          WEVE              6
136800160209     C**                 MOVE      wcev          WEVE
136900150723
137000160209     C***                Z-ADD     1             YY                5 0
137100160209     C**   LAB(XY)       IFEQ      'S'
137200160209     C***  WEVE          LOOKUP    ELB(YY)                                32
137300160209     C***                MOVEL     DLB(YY)       DSEM
137400160209     C***                ELSE
137500160209     C***  WEVE          LOOKUP    EVG(YY)                                32
137600160209     C***                MOVEL     DVG(YY)       DSEM
137700160209     C***                END
137800150723
137900150723     C                   MOVE      §EMDES        DI1DEV
138000150723     C                   MOVE      §EMDES        DI1DEV
138100150723     C                   MOVE      §EMCOD        DI1C2A
138200150723     C                   MOVE      §EMCCA        DI1CCA
138300150723     C                   MOVE      §EMBLP        DI1BLP
138400150723     C                   MOVE      §EMBLT        DI1BLT
138500150723     C                   MOVE      §EMFOR        DI1FRZ
138600150723     C                   MOVE      §EMTOT        DI1TOT
138700150723     C                   MOVE      BLPNCL        DI1NCL
138800150723     c                   else
138900980203     C                   MOVEL     V2CFTX        DI1FTX
139000980203     C                   MOVEL     V2CKSC        DI1KSC
139100970612     C                   MOVEL     V2DKSC        DI1DEC
139200970612     C                   MOVEL     V2CLNP        DI1LNP
139300970612     C                   MOVEL     V2CNRS        DI1NRS
139400970612     C                   MOVEL     V2CNSP        DI1NSP
139500970612     C                   MOVEL     V2CAAS        DI1AAS
139600970612     C                   MOVEL     V2CAAS        DI1DSP
139700970612     C                   MOVE      BLPMGS        DI1DSP
139800970616     C                   MOVE      BLPMGS        DI1DSP
139900971007     C                   MOVE      LAB(XY)       DI1LAB
140000150723     C                   MOVE      V2CSTS        DI1STS
140100150723     C                   MOVE      V2CCOD        DI1CEV
140200980202     C                   MOVE      §EMDES        DI1DEV
140300980203     C                   MOVE      §EMDES        DI1DEV
140400980203     C                   MOVE      §EMCOD        DI1C2A
140500980203     C                   MOVE      §EMCCA        DI1CCA
140600980203     C                   MOVE      §EMBLP        DI1BLP
140700980203     C                   MOVE      §EMBLT        DI1BLT
140800980203     C                   MOVE      §EMFOR        DI1FRZ
140900980203     C                   MOVE      §EMTOT        DI1TOT
141000980204     C                   MOVE      WDAFOR        DI1FOR
141100980204     C                   MOVE      BLPNCL        DI1NCL
141200150723     c                   endif
141300970612     C                   MOVEL     FNLSI1        KPJBU
141400980202     C* Richiamo pgm gestione dettaglio segnacolli
141500980202     C     §EMTOT        IFNE      'S'
141600980202     C                   CALL      'FNLSI1R2'
141700980202     C                   PARM                    KPJBA
141800980202     C                   MOVEL     KPJBU         FNLSI1
141900980202     C                   END
142000980202     C* Se ho digitato F3 esco
142100980202     C     DI1CMD        IFEQ      '03'
142200980202     C                   MOVEL     'S'           WFINE
142300980202     C                   GOTO      FINRIC
142400980202     C                   END
142500980204     C* Se ho digitato F12 azzero campo scelta
142600980204     C     DI1CMD        IFEQ      '12'
142700980204     C                   GOTO      FINRIC
142800980204     C                   END
142900980202     C* Richiamo pgm gestione terza videata: SPEDIZIONE
143000160603     c                   if        wtpvid='S' and *inkb
143100160603     C                   CALL      'FNLSI1R1'
143200160603     C                   PARM                    KPJBA
143300160603     C                   PARM      'N'           par_vid           1
143400160603     c                   else
143500970612     C                   CALL      'FNLSI1R1'
143600970612     C                   PARM                    KPJBA
143700160603     c                   endif
143800970617     C                   MOVEL     KPJBU         FNLSI1
143900970612     C*
144000970612     C* Se ho digitato F3 esco
144100970612     C                   SELECT
144200970612     C     DI1CMD        WHENEQ    '03'
144300970619     C                   MOVEL     'S'           WFINE
144400970619     C* Controllo se devo richiamare TRTC81
144500970619     C     WRIC81        IFEQ      'S'
144600970619     C                   EXSR      CALL81
144700970619     C                   END
144800970612     C* Se ho digitato F12 azzero campo scelta
144900970612     C     DI1CMD        WHENEQ    '12'
145000970616     C* CONFERMA
145100970616     C     DI1CMD        WHENEQ    '06'
145200970617     C                   MOVEL     'S'           WRIC81
145300970616     C* Aggiorno BLP/BLT
145400980202     C     §EMBLP        IFEQ      'S'
145500970612     C                   CALL      'FNLSI1R3'
145600970617     C                   PARM                    KPJBA
145700970616     C                   END
145800080421      * l'EDIVAC viene sostituito con il TIVGD Download standard
145900080421     C* Scrivo EDVAC --> TIVGD00F
146000970612     C                   CALL      'FNLSI1R4'
146100970612     C                   PARM                    KPJBA
146200970616     C                   ENDSL
146300970612     C*
146400980202     C     FINRIC        ENDSR
146500970617     C*--------------------------------------------------------------*
146600980126     C* CALL81: PER E.D.I. RICHIAMO PGM TRTC81C
146700970617     C*--------------------------------------------------------------*
146800970617     C     CALL81        BEGSR
146900970617     C*
147000980126      * CALL A TRTC81C PER E.D.I.
147100980126     C     §PTFTT        IFNE      'S'
147200061124     C     §PTFTT        ANDNE     'P'
147300980126     C                   MOVEL     'S'           WCAL81            1
147400080421      *
147500080421      *  Scrive IFTSTA x EDI:
147600080421      * ? il TRTC81C  attiva giro di trasm con SNDIFTSTA  ?
147700080421     C                   MOVEL     *all'0'       Elab_Cliente
147800080421     C                   MOVE      CKS(XX)       Elab_Cliente
147900080421     c                   eval      Tipo_Scarico = CSD(XX)
148000080421      *
148100080421     c                   eval      Tipo_File = 'VC'
148200110630     C                   CALL      'TRTCT81C'
148300080421     c                   parm                    Elab_Cliente      8
148400080422     c                   parm                    Tipo_File         2
148500080421     c                   parm                    Tipo_Scarico      2
148600980126      *
148700980126     C                   END
148800980126      *
148900970617     C                   ENDSR
149000970410     C*--------------------------------------------------------------*
149100970410     C* *INZSR: Operazioni iniziali
149200970410     C*--------------------------------------------------------------*
149300970410     C     *INZSR        BEGSR
149400970410     C*
149500970410     C     *ENTRY        PLIST
149600970410     C                   PARM                    KPJBA
149700990223     C* Richiamo xparut
149800970410     C                   Z-ADD     1             CODUT
149900970410     C                   CALL      'X§PARUT'
150000970410     C                   PARM                    UTEDSE
150100970410     C                   MOVEL     REC80         CNCR80
150200970410     C                   MOVEL     RAGUT         RSUT
150300030924      *
150400030924      *  In base all'utente prende il P.O.
150500030924     C                   CLEAR                   Tibs36Ds
150600030924     c                   EVAL      I36ute = Knmus
150700030924     c                   EVAL      I36Tla = 'L'
150800030924     C                   CALL      'TIBS36R'
150900030924     C                   PARM                    Tibs36Ds
151000030924      *
151100970612     C*--- RICERCA CAPOCONTI------------------------------------------*
151200030116     C                   DO        50            X                 5 0
151300970612     C                   MOVE      TCU(X)        TCUDS
151400970612     C     F56           IFEQ      'CG'
151500970612     C     F4            ANDEQ     '1'
151600970612     C     F3            ANDEQ     '0'
151700970612     C                   Z-ADD     KCU(X)        CLIITA            4 0
151800970612     C                   END
151900970612     C                   END
152000970612     C*--- RICERCA CAPOCONTI------------------------------------------*
152100970410      *  Definizione chiavi
152200970410     C     KBLP          KLIST
152300970410     C                   KFLD                    KAAS
152400970410     C                   KFLD                    KLNP
152500970410     C                   KFLD                    KNRS
152600970410     C                   KFLD                    KNSP
152700970410     C     KBLT          KLIST
152800970410     C                   KFLD                    KAAS
152900970410     C                   KFLD                    KLNP
153000970410     C                   KFLD                    KNRS
153100970410     C                   KFLD                    KNSP
153200970410     C     KLBL1         KLIST
153300970410     C                   KFLD                    KAAP
153400970410     C                   KFLD                    KLPP
153500970410     C                   KFLD                    KNRP
153600970410     C                   KFLD                    KNSP
153700970410     C     KLBL2         KLIST
153800970410     C                   KFLD                    KAAN
153900970410     C                   KFLD                    KLPN
154000970410     C                   KFLD                    KNRN
154100970410     C                   KFLD                    KNSN
154200970410     C     KTAB1         KLIST
154300970410     C                   KFLD                    KKUT
154400970410     C                   KFLD                    KCOD
154500970410     C     KTAB2         KLIST
154600970410     C                   KFLD                    KKUT
154700970410     C                   KFLD                    KCOD
154800970410     C                   KFLD                    KKEY
154900970612     C     KACO          KLIST
155000970612     C                   KFLD                    KKUT
155100970612     C                   KFLD                    KKCC
155200970612     C                   KFLD                    KKSC
155300980204     C     KEVB          KLIST
155400980204     C                   KFLD                    KAA2
155500980204     C                   KFLD                    KLNP
155600980204     C                   KFLD                    KNRS
155700980204     C                   KFLD                    KNSP
155800980204     C                   KFLD                    KCEV
155900080918     C     KTBE1         KLIST
156000080918     C                   KFLD                    TBECOD
156100080918     C                   KFLD                    TBEKE1
156200150415     C     Kedtab        KLIST
156300150415     C                   KFLD                    tabCOD
156400150415     C                   KFLD                    tabKEY
156500150724     C     KBrv          KLIST
156600150724     C                   KFLD                    bltfls
156700150724     C                   KFLD                    bltlna
156800150724     C                   KFLD                    bltNRS
156900150724     C                   KFLD                    bltnsc
157000160308     C     Kar4          KLIST
157100160308     C                   KFLD                    KAAS
157200160308     C                   KFLD                    KLNP
157300160308     C                   KFLD                    KNRS
157400160308     C                   KFLD                    KNSP
157500160308     C                   KFLD                    Ktrc
157600160308     C     Korm          KLIST
157700160308     C                   KFLD                    §B4POE
157800160308     C                   KFLD                    §B4NSR
157900160308     C                   KFLD                    §B4NOR
158000160308     C                   KFLD                    §B4NRV
158100970410      *  Definizione variabili
158200980204     C     *LIKE         DEFINE    EVBAAS        KAA2
158300980204     C     *LIKE         DEFINE    EVBCEV        KCEV
158400970410     C     *LIKE         DEFINE    BLPAAS        KAAS
158500970410     C     *LIKE         DEFINE    BLPLNP        KLNP
158600970410     C     *LIKE         DEFINE    BLPNRS        KNRS
158700970410     C     *LIKE         DEFINE    BLPNSP        KNSP
158800970410     C     *LIKE         DEFINE    LBLAAP        KAAP
158900970410     C     *LIKE         DEFINE    LBLLPP        KLPP
159000970410     C     *LIKE         DEFINE    LBLNRP        KNRP
159100970410     C     *LIKE         DEFINE    LBLAAN        KAAN
159200970410     C     *LIKE         DEFINE    LBLLPN        KLPN
159300970410     C     *LIKE         DEFINE    LBLNRN        KNRN
159400970410     C     *LIKE         DEFINE    LBLNSN        KNSN
159500970410     C     *LIKE         DEFINE    TBLKUT        KKUT
159600970410     C     *LIKE         DEFINE    TBLCOD        KCOD
159700970410     C     *LIKE         DEFINE    TBLKEY        KKEY
159800970611     C     *LIKE         DEFINE    TABCOD        KCODE
159900970612     C     *LIKE         DEFINE    ACOKCC        KKCC
160000970612     C     *LIKE         DEFINE    ACOKSC        KKSC
160100970410      *
160200970410      *  Reperisco la data del giorno
160300970410     C                   TIME                    WHHDAT           14 0
160400970414     C                   MOVE      WHHDAT        G02DAT
160500970414     C                   MOVEL     *BLANK        G02ERR
160600970414     C                   CALL      'XSRDA8'
160700970414     C                   PARM                    WLBDA8
160800970414     C                   Z-ADD     G02INV        OGGI              8 0
160900970611     C*
161000970611     C* Carico dati DS partner in schiera
161100970612     C                   OPEN      EDTAB01L
161200150415     C                   Z-ADD     0             X
161300970611     C                   MOVEL     'PT'          KCODE
161400970616     C     KCODE         CHAIN     EDTAB01L                           30
161500970611     C     *IN30         DOWEQ     '0'
161600970611     C     TABFLG        IFEQ      ' '
161700970611     C                   ADD       1             X
161800970611     C                   MOVEL     TABUNI        DSPT
161900150415     C                   MOVEL     TABKEY        KEYPT(X)
162000970611     C                   MOVEL     §PTKSC        KSC(X)
162100970616     C                   MOVEL     §PTLNP        LNP(X)
162200970616     C                   MOVEL     §PTNRS        NRS(X)
162300970611     C                   END
162400970616     C     KCODE         READE     EDTAB01L                               30
162500970611     C                   END
162600970616     C*
162700150415     C* x evitare che a causa di un disallineamento imprevisto
162800150415     C*  la PU possa essere disallineata alla PT  eseguo un caricamento
162900150415     C*  differente.
163000150415     c                   clear                   lab
163100150415     c                   do        x             pux               5 0
163200150415     C                   MOVEL     'PU'          tabCOD
163300150415     C                   MOVEL     KEYPT(pux)    tabKEY
163400150415     C     Kedtab        CHAIN     EDTAB01L
163500150415     c                   if        %Found(edtab01l) and tabflg =' '
163600150415      *  Lista Labarta
163700150415     C                   MOVEL     TABUNI        DSPU
163800150415     C                   MOVEL     §PULAB        LAB(pux)
163900150415     c                   end
164000150415     c                   enddo
164100150415      *
164200150415      **** vecchio caricamento
164300150415     C****               Z-ADD     0             X
164400150415     C****               MOVEL     'PU'          KCODE
164500150415     C**** KCODE         CHAIN     EDTAB01L                           30
164600150415     C**** *IN30         DOWEQ     '0'
164700150415     C**** TABFLG        IFEQ      ' '
164800150415     C****               ADD       1             X
164900150415     C****               MOVEL     TABUNI        DSPU
165000150415     C****               MOVEL     §PULAB        LAB(X)
165100150415     C****               END
165200150415     C**** KCODE         READE     EDTAB01L                               30
165300150415     C****               END
165400150415      *
165500150415     C*  Carico in schiera codici collegati - CL -
165600150415     C                   Z-ADD     0             X
165700150415     C                   MOVEL     'CL'          KCODE
165800150415     C     KCODE         CHAIN     EDTAB01L                           30
165900150415     C     *IN30         DOWEQ     '0'
166000150415     C     TABFLG        IFEQ      ' '
166100150415     C                   ADD       1             X
166200150415     C                   MOVEL     TABUNI        DSCL
166300150415     C                   MOVEL     §CLKSC        clKSC(X)
166400150415     C                   MOVEL     §CLLNP        clLNP(X)
166500150415     C                   END
166600150415     C     KCODE         READE     EDTAB01L                               30
166700150415     C                   END
166800980202     C*
166900980202     C* Carico in schiera tabella eventi manuali
167000150723     c                   clear                   wcev
167100150723     c                   clear                   wsts
167200980202     C                   Z-ADD     0             X
167300030116     C                   Z-ADD     0             X1                5 0
167400980202     C                   MOVEL     'EM'          KCODE
167500980202     C     KCODE         CHAIN     EDTAB01L                           30
167600980202     C     *IN30         DOWEQ     '0'
167700980202     C     TABFLG        IFEQ      ' '
167800980202     C                   MOVEL     TABUNI        DSEM
167900980202     C     §EMLAB        IFEQ      'S'
168000980202     C                   ADD       1             X
168100980204     C                   MOVEL     TABKEY        ELB(X)
168200980202     C                   MOVEL     TABUNI        DLB(X)
168300980202     C                   END
168400980202     C     §EMVGL        IFEQ      'S'
168500980202     C                   ADD       1             X1
168600980204     C                   MOVEL     TABKEY        EVG(X1)
168700980202     C                   MOVEL     TABUNI        DVG(X1)
168800980202     C                   END
168900150723     c* memorizzo il codice dell'evento corrispondente alla chiusura bolla
169000150723     c* per merce mai affidata
169100150723     c                   if        §emmai='S'
169200150723     c                   movel     tabkey        wsts
169300150723     c                   eval      wcev=%subst(tabkey:4:3)
169400160209     c                   eval      s_dsem=dsem
169500150723     c                   endif
169600980202     C     KCODE         READE     EDTAB01L                               30
169700980202     C                   END
169800980202     C                   END
169900970611     C* close EDTAB
170000970611     C                   CLOSE     EDTAB01L
170100970611     C*
170200970410     C* Eseguo caricamento tabella 3K
170300160407     C****               OPEN      TABEL00F
170400970410     C                   Z-ADD     0             X
170500970410     C                   Z-ADD     1             KKUT
170600970410     C                   MOVEL     '3K'          KCOD
170700970410     C     KTAB1         CHAIN     TABEL                              30
170800970410     C     *IN30         DOWEQ     '0'
170900030213     C     X             ANDLT     9990
171000970410     C     TBLFLG        IFEQ      *BLANKS
171100970410     C                   ADD       1             X
171200970617     C                   MOVEL     TBLUNI        DS3K
171300970617     C                   MOVEL     TBLKEY        C3K(X)
171400970617     C                   MOVEL     §3KCKS        CKS(X)
171500010725     C                   MOVEL     §3KCSD        CSD(X)
171600970410     C                   END
171700970410     C     KTAB1         READE     TABEL                                  30
171800970410     C                   END
171900970715     C* Eseguo caricamento tabella 3A
172000970721     C                   Z-ADD     0             X
172100970715     C                   MOVEL     '3A'          KCOD
172200970715     C     KTAB1         CHAIN     TABEL                              30
172300970715     C     *IN30         DOWEQ     '0'
172400970721     C     X             ANDLT     500
172500970715     C     TBLFLG        IFEQ      *BLANKS
172600970715     C                   ADD       1             X
172700970715     C                   MOVEL     TBLUNI        DS3A
172800970715     C                   MOVEL     TBLKEY        C3A(X)
172900970715     C                   MOVEL     §3ATB1        TPO(X)
173000970715     C                   END
173100970715     C     KTAB1         READE     TABEL                                  30
173200970715     C                   END
173300970701     C*
173400970701     C* Eseguo caricamento da tabella 7J codici pistole fittizie
173500970701     C                   MOVEA     *HIVAL        C7J
173600970701     C                   Z-ADD     0             X
173700970701     C                   Z-ADD     1             KKUT
173800970701     C                   MOVEL     '7J'          KCOD
173900970701     C     KTAB1         CHAIN     TABEL                              30
174000970701     C     *IN30         DOWEQ     '0'
174100970721     C     X             ANDLT     500
174200970701     C     TBLFLG        IFEQ      *BLANKS
174300970701     C                   MOVEL     TBLUNI        DS7J
174400970912     C     §7JRPS        IFEQ      'A'
174500970701     C                   ADD       1             X
174600970701     C                   MOVEL     TBLKEY        C7J(X)
174700970701     C                   END
174800970701     C                   END
174900970701     C     KTAB1         READE     TABEL                                  30
175000970701     C                   END
175100150728
175200150728     C                   MOVEL     '3I'          Kcod
175300150728     C                   MOVEL     '1       '    kKEY
175400150728     c                   clear                   ds3i
175500150728     C     KTAB2         CHAIN     TABEL00F
175600150728     c                   if        %found(tabel00f)
175700150728     C                   MOVEL     TBLUNI        DS3I
175800150728     c                   endif
175900970410     C* Eseguo caricamento £1
176000020506     c                   clear                   trul06ds
176100020506     c                   eval      d06cod = '£1'
176200030924     c                   movel     o36pou        d06key
176300020506     c                   movel     trul06ds      kpjbu
176400020506     c                   call      'TRUL06R'
176500020506     c                   parm                    kpjba
176600020506     c                   movel     kpjbu         trul06ds
176700020506     c                   movea     lin           £1
176800150724     c* terminal di partenza del p.o. utente
176900150724     c                   clear                   fnlv55ds
177000150724     C                   MOVEL     'P'           D55TPT
177100150724     C                   MOVEL     simpou        D55LIN
177200150724     C                   z-add     oggi          D55drf
177300150724     C                   CALL      'FNLV55R'
177400150724     C                   PARM                    fnlv55ds
177500150724     c                   z-add     d55tfp        wtfp
177600020912      *
177700971008     C* Imposto a 'N' flag richiamo TRTC81C
177800971008     C                   MOVEL     'N'           WCAL81            1
177900970410     C* Eseguo close del file
178000160407     C****               CLOSE     TABEL00F
178100970410     C*
178200970410     C                   ENDSR
178300080930      *------------------------------------------------------------------------*
178400080930      * Routine di controllo x eventuali Errori
178500080930      *------------------------------------------------------------------------*
178600080930     C     *PSSR         BEGSR
178700080930      *
178800080930      * Comunque eseguo x rilasciare il TIVGD.
178900080930      *? Infine elimino il blocco elaborazione TIVGD x tipo file qui.  ?
179000080930     C                   clear                   trul47ds
179100080930     C                   eval      d47opz  = 'F'
179200080930     C                   eval      d47tip  = 'VC'
179300080930     C                   call      'TRUL47R'
179400080930     C                   parm                    trul47ds
179500080930      *
179600080930     C                   ENDSR     '*CANCL'
179700080930     C*------------------------------------------------------------------------*
179800971008**
179900970701Immettere codice cliente                                                      01
180000970701Codice cliente inesistente                                                    02
180100060811Codice cliente non presente in tabella 3K o PT                                03
180200970701Un altro utente sta chiudendo le spedizioni del cliente                       04
180300970701Immettere anno della spedizione                                               05
180400970701Immettere linea di partenza della spedizione                                  06
180500970701Linea di partenza inesistente                                                 07
180600970701Linea di partenza non gestita su questo as                                    08
180700970701Immettere numero della spedizione                                             09
180800970701Spedizione inesistente                                                        10
180900980206Bolla mamma: impossibile gestirla                                             11
181000980206Bolla figlia: impossibile gestirla                                            12
181100970701Spedizione già trasmessa alla filiale di arrivo                               13
181200970701Spedizione già trasmessa alla sede                                            14
181300970701Spedizione già trasmessa al cliente                                           15
181400980202Spedizione senza colli: impossibile eseguire chiusura                         16
181500980202Spedizione non appartenente al cliente selezionato                            17
181600980202Il codice selezionato deve appartenere ad un partner                          18
181700980202Codice evento inesistente                                                     19
181800160308Bolla in assegnato: occorre prima verificare ed annullare l'ORM               20
181900980202Codice evento obbligatorio                                                    21
182000980204Spedizione già consegnata                                                     22
182100980204Evento già inviato impossibile rinviarlo                                      23
182200990610Esistono C.A. aperte per la spedizione, codice evento non valido              24
182300131016 ATTENZIONE: più bolle con stesso riferimento!!                               25
182400131016 VERIFICARE se la Spedizione è quella corretta.                               26
182500150723Nessun dato da visualizzare per la selezione richiesta                        27
182600150728                                                                              28
182700150724Spedizione non gestibile:presenti spunte                                      29
182800150728Attenzione: caricate solo 9.999 bolle ma ne sono presenti ancora              30
182900150909Non trovato Evento per Chiusura per Merce mai Affidata. Contattare il CED (EM)31
183000160408Immettere una selezione                                                       32
183100160603Non tutte le spedizioni sono state chiuse: premere INVIO per verificare       33
