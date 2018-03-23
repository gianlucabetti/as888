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
003200970410     FTABEL00F  IF   E           K DISK    USROPN
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
007300150909     D ERR             S             78    DIM(31) CTDATA PERRCD(1)
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
008700970410     D*--------------------------------------------------------------*
008800970410     D* DS
008900970410     D*--------------------------------------------------------------*
009000030924     D Tibs36ds      E DS
009100970612     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
009200970612     D  TCU                  398    697
009300970612     D                                     DIM(50)
009400970612     D  KCU                  698    847P 0
009500970612     D                                     DIM(50)
009600970612     D                                     PACKEVEN
009700970612     D TCUDS           DS
009800970612     D  F3                     3      3
009900970612     D  F4                     4      4
010000970612     D  F56                    5      6
010100970612     D CNCR80        E DS
010200020506     d trul06ds      e ds
010300020506     d  lin                    1     90  0 dim(30)
010400970612     D* DS  appoggio x cmd
010500970616     D CMDAPP          DS            50
010600970616     D  NOMMBR                42     48
010700970617     D CMDLOG          DS            80
010800970617     D  MBRLOG                42     48
010900970617     D  MBRFIS                71     77
011000150724     d fnlv55ds      e ds
011100970414     D WLBDA8          DS
011200970414     D  G02DAT                 1      8  0
011300970414     D  G02INV                 9     16  0
011400970414     D  G02ERR                17     17
011500970414     D  G02TGI                18     22  0
011600150728     d WDAT8           DS
011700150728     d  datada                 1      8  0
011800150728     d  dataa                  9     16  0
011900150728     d  ggl                   17     21  0
012000080421      *----------------------------------------------------------------
012100080421      * Controllo e semaforo x scrittura del VAC: TRUL47R
012200080421      *----------------------------------------------------------------
012300080421     D trul47ds      e ds
012400080421      *
012500970410     D KPJBA         E DS
012600150723     d fnblpds       e ds                  extname(fnblp00f) inz
012700970612     D DSPT          E DS                  EXTNAME(EDIDSPT)
012800970616     D DSPU          E DS                  EXTNAME(EDIDSPU)
012900980202     D DSEM          E DS                  EXTNAME(EDIDSEM)
013000160209     D s_dsem        E DS                  EXTNAME(EDIDSEM)
013100160209     D                                     prefix(S_)
013200150415     D DSCL          E DS                  EXTNAME(EDIDSCL)
013300970616     D* DATI X RICHIAMO PGM INTERR. BOLLE PARTENZE (FNLS05R)
013400970616     D PARA04          DS                  INZ
013500970616     D  P04AAS                 1      4  0
013600970616     D  P04LNP                 5      7  0
013700970616     D  P04NRS                 8      9  0
013800970616     D  P04NSP                10     16  0
013900980204     D  P04F03                17     17
014000970616     D  P04FLG                31     31
014100970616     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
014200970616     D  P04GIA                48     48
014300970616      * PA3CLI indica se il pgm e' richiamato da un cliente (='S')
014400970616     D  P04CLI               143    143
014500970616     D PARA05          DS                  INZ
014600970616     D  P05AAS                 1      4  0
014700970616     D  P05LNP                 5      7  0
014800970616     D  P05NRS                 8      9  0
014900970616     D  P05NSP                10     16  0
015000970616     D  P05F03                17     17
015100970616     D  P05FLG                18     18
015200970616     D  P05SIM                44     46  0
015300970616     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
015400970616     D  P05GIA                48     48
015500080421     D***** TIBS55        E DS                  EXTNAME(TIBS55DS)
015600971126     D FNLSI1        E DS                  EXTNAME(FNLSI1DS)
015700970410     D DS3K          E DS
015800970715     D DS3A          E DS
015900970701     D DS7J          E DS
016000970410     D DS7O          E DS
016100150728     D DS3i          E DS
016200060811     D OG143         E DS
016300160308     D dsbl4m        E DS
016400040715
016500040715     d Fidn12ds      e ds
016600040715     d  skKey                 26    305    dim(20)
016700040715     d  skAnn                306    325    dim(20)
016800040715     d  skDca                326    485    dim(20)
016900040715     d  skFca                486    545  0 dim(20)
017000040715     d  skDch                546    705  0 dim(20)
017100040715     d  skCch                706    745    dim(20)
017200040715
017300970410     D                SDS
017400970410     D  NOMPGM                 1     10
017500970410     I*--------------------------------------------------------------*
017600970410     I*  Definizione campi Azorg
017700970410     I*--------------------------------------------------------------*
017800150724     I***AZORG01L  AA
017900150724     I***                                  1    3  ORGDIT
018000150724     I***                             P    4    5 0ORGFIL
018100150724     I***                               3776 3800  ORGDE3
018200150728      /copy gaitrasrc/srcprotopr,xsrlav8
018300970410     C*--------------------------------------------------------------*
018400970410     C*  C I C L O     P R I N C I P A L E
018500970410     C*--------------------------------------------------------------*
018600150723      /free
018700150723         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
018800150723      /end-free
018900970410     C*  Inizializzo varibili videata
019000970410     C                   EXSR      INZ01
019100080421      *
019200970410     C*  Gestione della prima videata
019300970410     C     WFINE         DOWEQ     'N'
019400970611     C     WTPVID        CASEQ     '1'           GESD01
019500970611     C     WTPVID        CASEQ     '2'           GESD02
019600150723     C     WTPVID        CASEQ     'S'           GESsfl
019700970410     C                   END
019800970611     C                   END
019900970410     C*  Fine pgm
020000970410     C                   SETON                                        LR
020100970611     C*--------------------------------------------------------------*
020200970611     C* INZ01: Impostazione della prima videata
020300970611     C*--------------------------------------------------------------*
020400970611     C     INZ01         BEGSR
020500970611     C*
020600970611     C                   SETOFF                                       2840
020700970616     C                   MOVEL     '1'           WTPVID
020800970617     C                   MOVEL     'N'           WRIC81            1
020900970616     C                   MOVEL     'N'           WFINE
021000970611     C                   MOVEL     NOMPGM        V1CPGM
021100970611     C                   Z-ADD     0             V1CKSC
021200970611     C*
021300970611     C                   ENDSR
021400970410     C*--------------------------------------------------------------*
021500970611     C* INZ02: Impostazione della prima videata
021600970410     C*--------------------------------------------------------------*
021700970611     C     INZ02         BEGSR
021800970410     C*
021900970410     C                   SETOFF                                       284041
022000970611     C                   SETOFF                                       424344
022100970616     C                   MOVEL     '2'           WTPVID
022200970611     C                   MOVEL     NOMPGM        V2CPGM
022300970611     C                   MOVEL     V1CKSC        V2CKSC
022400970611     C                   MOVEL     ACORAG        V2DKSC
022500980204     C                   MOVEL     *BLANKS       V2DCOD
022600980204     C                   MOVEL     *BLANKS       V2CCOD
022700980202     C                   MOVEL     *BLANKS       V2CSTS
022800980202     C                   MOVEL     *BLANKS       V2CFTX
022900970611     C                   MOVE      WHHDAT        V2CAAS
023000130304      **  gestione cliente con Partner gestito da altra filiale
023100130304      **  vedi GWEISS con clienti sulla 277.
023200130304     c                   if        LNP_ALTRA_FIL = 0
023300971007     C                   Z-ADD     LNP(XY)       V2CLNP
023400130821      ******
023500130821     c**** Chiodo x Ducros da Eliminare fra 333 e 330
023600130821     c                   if        v2clnp =333
023700130821     c                   eval      v2clnp =330
023800130821     c                   end
023900130821      ******
024000130304     c                   else
024100130304     C                   eval        V2CLNP = LNP_ALTRA_FIL
024200130304     c                   end
024300971007     C                   Z-ADD     NRS(XY)       V2CNRS
024400970611     C                   Z-ADD     0             V2CNSP
024500970410     C*
024600970410     C                   ENDSR
024700970410     C*--------------------------------------------------------------*
024800970410     C* GESD01: Gestione della prima videata
024900970410     C*--------------------------------------------------------------*
025000970410     C     GESD01        BEGSR
025100970410     C*
025200970410     C* Eseguo exfmt della prima videata
025300970410     C                   EXFMT     LSI1D01
025400970611     C                   SETOFF                                       2840
025500970410     C                   SETOFF                                       4243
025600970410     C* Fine
025700970410     C     *INKC         IFEQ      '1'
025800970414     C                   MOVEL     'S'           WFINE             1
025900970410     C                   GOTO      FINVD1
026000970410     C                   END
026100970410     C* Controlli
026200970410     C                   EXSR      CTR01
026300970410     C   28              GOTO      FINVD1
026400970611     C*
026500970611     C* Se no errori passo a videata successiva
026600150723     c* chiusura bolle per merce mai affidata --> la videata successiva è il sfl
026700150723     c* delle bolle
026800150723     c                   if        v1cfbo='S'
026900150723     C                   MOVEL     'S'           WTPVID
027000150723     c                   eval      $inzS01=*on
027100150723     c                   else
027200150724     C                   EXSR      INZ02
027300150723     C                   MOVEL     '2'           WTPVID
027400150723     c                   endif
027500970410     C*
027600970410     C     FINVD1        ENDSR
027700970611     C*--------------------------------------------------------------*
027800970611     C* GESD02: Gestione della seconda videata
027900970611     C*--------------------------------------------------------------*
028000970611     C     GESD02        BEGSR
028100970611     C*
028200970611     C* Eseguo exfmt della prima videata
028300970611     C                   EXFMT     LSI1D02
028400970611     C                   SETOFF                                       284041
028500970611     C                   SETOFF                                       424344
028600970611     C* Fine
028700970611     C     *INKC         IFEQ      '1'
028800970611     C                   MOVEL     'S'           WFINE             1
028900970617     C* Controllo se devo richiamare TRTC81
029000970617     C     WRIC81        IFEQ      'S'
029100970617     C                   EXSR      CALL81
029200970617     C                   END
029300970611     C                   GOTO      FINVD2
029400970611     C                   END
029500970611     C* Prima videata
029600970611     C     *INKL         IFEQ      '1'
029700970611     C                   MOVEL     '1'           WTPVID            1
029800970617     C* Controllo se devo richiamare TRTC81
029900970617     C     WRIC81        IFEQ      'S'
030000970617     C                   EXSR      CALL81
030100970617     C                   END
030200970611     C                   GOTO      FINVD2
030300970611     C                   END
030400080421      *
030500970612     C* Interrogazione bolle
030600970612     C     *INKG         IFEQ      '1'
030700970616     C     V2CNSP        IFGT      0
030800970616     C                   CLEAR                   PARA05
030900970616     C                   Z-ADD     V2CAAS        P05AAS
031000970616     C                   Z-ADD     V2CLNP        P05LNP
031100970616     C                   Z-ADD     V2CNRS        P05NRS
031200970616     C                   Z-ADD     V2CNSP        P05NSP
031300970616     C                   MOVEL     '1'           P05GIA
031400970616     C                   MOVEL     PARA05        KPJBU
031500970616     C                   CALL      'FNLS05R'
031600970616     C                   PARM                    KPJBA
031700970616     C                   MOVEL     KPJBU         PARA05
031800980204     C     P05F03        IFEQ      *BLANKS
031900980204     C                   Z-ADD     P05AAS        V2CAAS
032000980204     C                   Z-ADD     P05LNP        V2CLNP
032100980204     C                   Z-ADD     P05NRS        V2CNRS
032200980204     C                   Z-ADD     P05NSP        V2CNSP
032300980204     C                   ENDIF
032400970616     C*
032500970616     C                   ELSE
032600970616     C*
032700970616     C                   CLEAR                   PARA04
032800970616     C                   MOVEL     '1'           P04FLG
032900970616     C                   MOVEL     PARA04        KPJBU
033000970616     C                   CALL      'FNLS04R'
033100970616     C                   PARM                    KPJBA
033200980204     C                   MOVEL     KPJBU         PARA04
033300980204     C     P04F03        IFEQ      *BLANKS
033400980204     C                   Z-ADD     P04AAS        V2CAAS
033500980204     C                   Z-ADD     P04LNP        V2CLNP
033600980204     C                   Z-ADD     P04NRS        V2CNRS
033700980204     C                   Z-ADD     P04NSP        V2CNSP
033800970616     C                   ENDIF
033900980204     C                   ENDIF
034000970612     C                   GOTO      FINVD2
034100970612     C                   END
034200970611     C* Controlli
034300970611     C                   EXSR      CTR02
034400970611     C   28              GOTO      FINVD2
034500970611     C*
034600970611     C* Se no errori vado a pgm successivo
034700970611     C                   EXSR      CALPGM
034800970616     C* Azzero prima videata se ho eseguito conferma
034900970616     C     DI1CMD        IFEQ      '06'
035000970617     C                   EXSR      INZ02
035100970616     C                   END
035200970611     C*
035300970611     C     FINVD2        ENDSR
035400970611     C*--------------------------------------------------------------*
035500970611     C* CTR01: Controlli prima videata
035600970611     C*--------------------------------------------------------------*
035700970611     C     CTR01         BEGSR
035800970611     C*
035900970611     C* Controllo se immesso codice cliente
036000970611     C     V1CKSC        IFEQ      0
036100970611     C                   SETON                                        2840
036200970611     C                   MOVEL     ERR(1)        $MSG
036300970611     C                   END
036400990610     C   28              GOTO      FINCT1
036500150415      *
036600970611     C* Controllo se esiste cliente
036700970612     C                   Z-ADD     1             KKUT
036800970611     C                   Z-ADD     CLIITA        KKCC
036900970611     C                   Z-ADD     V1CKSC        KKSC
037000970611     C     KACO          CHAIN     CNACO00F                           31
037100970611     C     *IN31         IFEQ      '1'
037200150723     c                   clear                   vhdksc
037300970611     C                   SETON                                        2840
037400970611     C                   MOVEL     ERR(2)        $MSG
037500970611     C                   END
037600990610     C   28              GOTO      FINCT1
037700150723     c                   movel     acorag        vhdksc
037800150415      ****
037900970617     C* Controllo Codice cliente unificante tabella 3K
038000030116     C                   Z-ADD     1             XX                5 0
038100970617     C     V1CKSC        LOOKUP    C3K(XX)                                32
038200060811      *
038300060811      **?se non ha agganciato nulla
038400060811     c                   if        *in32 = *off
038500150415      *
038600060811     c     v1cksc        div       10000         trelnp            3 0
038700060811     C                   Z-ADD     treLNP        ORGFIL
038800060811     C     ORGFIL        CHAIN     AZORG01L                           31
038900060811      **? e se  si tratta di DPD (eccezione .......)
039000060811     c                   if        %Found(AZORG01L)
039100060811     c                   movel     orgde3        og143
039200060811     c                   if        §OGNTW ='DPD'
039300060811     C                   Z-ADD     1             XY                5 0
039400150415      ** va direttamente sulla PT poichè DPD non sta sulla 3K
039500060811     C     V1CKSC        LOOKUP    KSC(XY)                                32
039600060811     c                   end
039700060811     c                   end
039800060811      **
039900060811     c                   else
040000060811      **
040100150415      **  con l'unificante della 3K
040200970617     C* Controllo se cliente in tabella PT
040300060811     C                   Z-ADD     1             XY                5 0
040400060811     C     CKS(XX)       LOOKUP    KSC(XY)                                32
040500150415      **
040600060811     c                   end
040700060811      **
040800150415      **  Se non l'avesse trovato con l'UNIFICANTE sulla PT
040900970611     C     *IN32         IFEQ      '0'
041000150415      **
041100970611     C                   SETON                                        2840
041200970611     C                   MOVEL     ERR(3)        $MSG
041300150415      **
041400970617     C                   ELSE
041500020912      *
041600020912      *  la Linea di Partenza del Partner deve essere gestita
041700020912      *   nella £1
041800020912     C     LNP(XY)       lookup    £1                                     32
041900080918      ** ----
042000130304      **     Potrebbe essere un cliente appartenente alla tabella CL
042100130304      **     gestito da un'altra filiale. a questo punto occorre controllare,
042200130304      **     non solo la LINEA del PARTNER agganciata sull'unificante della 3K
042300130304      **     ma se non fosse gestito il Partner sulla filiale si deve vedere
042400130304      **     la Linea del Cliente se è gestita sulla filiale.
042500130304      **   (Esempio su clienti di GWeiss della 360 dati in gestione alla 277.
042600130304      **    il cliente è il 2772159 unificato sulla 3K al 3600001.
042700130304      **   Non è gestita la 360 nella 277 allora il cliente 2772159 è della 277
042800130304      **    quindi deve passare il controllo...poichè il cliente è in gestione)
042900130304     c                   clear                   LNP_ALTRA_FIL     3 0
043000150415      **
043100130304     c                   if        NOT *in32
043200150415      **
043300150415      ** Dovendo gestire casi in cui il cliente è stato spostato ANCORA di
043400150415      **  Filiale mantenendo il codice cliente, occorre effettuare il
043500150415      ** ragionamento sulla LNP presente sulla tab.CL o sulla PT per
043600150415      ** verificare l'appartenenza alla £1 e NON con i primi 3 caratteri del
043700150415      ** codice cliente.
043800150415      ** E'sempre il caso della GWEISS che adesso da 277 viene spostato alla
043900150415      **  249 mantenendo il codice cliente come 277xxxx e facendo capo alla 360
044000150415      **
044100150415     c                   clear                   treLNP            3 0
044200150415     c**** v1cksc        div       10000         treLNP            3 0
044300150415      ** ---
044400150415      **  cerca sulla CL  x impostare la LNP
044500150415     C                   Z-ADD     1             clpt              5 0
044600150415     C     V1CKSC        LOOKUP    clKSC(clpt)                            32
044700150415     c                   if        *in32
044800150415     c                   z-add     clLNP(clpt)   treLNP
044900150415     c                   else
045000150415      **  se non la trova cerca sulla PT  x impostare la LNP
045100150415     C                   Z-ADD     1             clpt              5 0
045200150415     C     V1CKSC        LOOKUP    KSC(clpt)                              32
045300150415     c   32              z-add     LNP(clpt)     treLNP
045400150415     c                   end
045500150415      ** ---
045600150415      **
045700130304     C     treLNP        lookup    £1                                     32
045800130304     c   32              movel     treLNP        LNP_ALTRA_FIL
045900150415      **
046000130304     c                   end
046100130304      ** ----
046200080918      ** Occorre anche controllare se DPD di backup quindi linee collegate
046300080918      **  per potere gestire linee da filiali diverse
046400080918      **   ossia tab."3NN"
046500080918      **
046600080918     C     *IN32         ifeq      *off
046700080918     c                   movel(p)  '3NN'         TbeCOD
046800080918     c                   movel(p)  KSC(xy)       TbeKE1
046900080918     c     kTBE1         setll     tntbe01l
047000080918     c     kTBE1         reade     tntbe01l
047100080918     C                   dow       not %EoF(tntbe01l)
047200080918     c                   movel(p)  tbeKE2        LNPAlternativa    3 0
047300080918     C     LNPAlternativalookup    £1                                     32
047400080918     c                   if        *in32
047500080918     c                   z-add     1             z                 3 0
047600080918     c     *zero         lookup    £1(z)                                  21
047700080918      * aggiunge la linea x il controllo successivo
047800080918     c                   if        *in21
047900080918     C                   z-add     lnp(xy)       £1(z)
048000080918     C                   End
048100080918     C                   Leave
048200080918     C                   End
048300080918     c     kTBE1         reade     tntbe01l
048400080918     C                   End
048500080918     C                   End
048600080918      ** ----
048700020912     C     *IN32         ifeq      *off
048800020912     C                   SETON                                        2840
048900020912     C                   MOVEL     err(8)        $MSG
049000020912     C                   Else
049100020912     C*
049200020912     C                   EndIF
049300020912     C*
049400970617     C                   END
049500020912     C*
049600990610     C   28              GOTO      FINCT1
049700150909     c* Se richiesta visualizzazione bolle da chiudere per merce mai
049800150909     c* affidata ERRORE se non trovato l'evento su tabella EM
049900150909     c                   if        v1cfbo='S' and wsts=*blanks
050000150909     C                   SETON                                        28
050100150909     C                   MOVEL     err(31)       $MSG
050200150909     c                   endif
050300080421     C*
050400970611     C     FINCT1        ENDSR
050500150723     C*--------------------------------------------------------------*
050600150723     C* gessfl:  videata sfl per codice cliente
050700150723     C*--------------------------------------------------------------*
050800150723     C     gessfl        BEGSR
050900150723      /free
051000160209         // Valorizzo DSEM con i dati dell'evento di chisura per merce mai aff.
051100160209         dsem=s_dsem ;
051200160209
051300150723         if $inzs01=*on;
051400150723            exsr inzs01;
051500150723            c01csr=1;
051600150723            c01rcd=1;
051700150723            $inzs01=*off;
051800150723         // Se non trovati record errore in prima videata
051900150723            if s01nrr=0   ;
052000150723               $msg=err(27)  ;
052100150723               *in28=*on  ;
052200150723               *in40=*on  ;
052300150723               wtpvid='1'  ;
052400150723               leavesr   ;
052500150723            endif  ;
052600150723            if s01nrr=1;
052700150724              chain (vshaas:vshlnp:vshnrs:vshnsp) fnblp01l ;
052800160308              ctrvid=*on ;
052900160308              exsr ctr_sfl   ;
053000160308              if not *in28  ;
053100160308                 exsr calpgm;
053200160308                 wtpvid='1' ;
053300160308                 leavesr;
053400160308              endif;
053500150723            endif;
053600150723         endif;
053700150723       // c01csr   contiene il numero di riga del subfile su cui era posizionato
053800150723       // impostando c01rcd   visualizzo la pagina che vedeva l'utente quando ha
053900150723       // l'ultimo tasto
0540001507232         if c01csr   > 0;
054100150723           c01rcd   = c01csr;
054200150723          else;
054300150723           c01rcd   = s01rcd   ;
0544001507232         endif;
054500150723       // se non so quale pagina visualizzare forzo pagina 1
0546001507232         if c01rcd   < 1;
054700150723           eval c01rcd   = 1;
0548001507232         endif;
054900150723
055000150723       // valorizzo campi del sfl control
055100150723          vc1ksc=%int(v1cksc);
055200150724          vc1kscd=vhdksc;
055300150723
055400150723
055500150723         // Emissione videata
055600150723         write  lsi1z01;
055700150723         exfmt  lsi1C01;
055800150723
055900150723         s01nrr=1  ;
056000150723
056100150723 1       SELECT;
056200150723
056300150723         // - F3=Fine
056400150723 1         WHEN  *inkc;
056500150723               wFine = 'S';
056600150723
056700150723         // - F12=Ritorno
056800150723 1         WHEN  *inkl;
056900150914               *in28=*off;
057000150723               wtpvid='1'  ;
057100150723
057200150723
057300150723         // -
057400150723 1       //WHEN  *inkb ;
057500150723         //  exsr seltot;
057600150723
057700150723
057800150723 x1      // Invio
057900150723           OTHER;
058000150723
058100150723               exsr ContrS01 ;
058200150723
058300150723 1       ENDSL;
058400150723      /end-free
058500150723     c                   endsr
058600150723      /free
058700150723       //--------------------------------------------------------------
058800150723       BEGSR InzS01;
058900150723
059000150723       // Pulizia subfile
059100150723         *in30  = *on;
059200150723         *in31  = *on;
059300150723         write  lsi1c01;
059400150723         *in31  = *off;
059500150723         *in30  = *off;
059600150723         *in32  = *off;
059700150723
059800150723         exec sql close BLPcsr;
059900150723       // Richiamo routine di preparazione stringa SQL
060000150723             exsr sr_prepsql;
060100150723
060200150723         S01nrr=0 ;
060300150723         clear $msg;
060400150723         *in28  = *off;
060500150723         *in90  = *off;
060600150728         *in19=*off;
060700160308         *in45=*off;
060800150723       // Elaboro i records estratti
060900150723           $finerec=*off;
061000150723           exec sql prepare STRINGASQL from :WrkStringaSql;
061100150723           exec sql declare BLPCSR cursor for StringaSql;
061200150723           exec sql open BLPcsr;
061300150723           dow $finerec=*off;
061400150723              exec sql Fetch BLPcsr into :fnblpds;
061500150728              *in28  = *off;
061600150728              if sqlcod=100 or sqlcod<0;
061700150723                 $finerec = *on;
061800150723                 leave;
061900150723              endif;
062000150728              if s01nrr=9999;
062100150728                 $finerec = *on;
062200150728                 $msg=err(30);
062300150728                 *in28=*on;
062400150728                 leave;
062500150728              endif;
062600150723              if %lookup(blplnp:£1)>0;
062700150724                 kaas=blpaas;
062800150724                 klnp=blplnp;
062900150724                 knrs=blpnrs;
063000150724                 knsp=blpnsp;
063100150728       //per poter chiudere la spedizione come merce mai affidata
063200150728       //devono essere trascorsi i giorni max di sospensione previsti
063300150728       //da tabella
063400150728                 clear wdat8;
063500150728                 datada=blpdim;
063600150728                 dataa=(blpaas*10000)+blpmgs;
063700150728                 xsrlav8(wdat8);
063800150728                 if (ggl-1) >  §3iggc;
063900160307                    ctrvid=*off;
064000150728                    exsr ctr_sfl;
064100150728                    if not *in28;
064200150728           // Carico record nel sfl
064300150728                       exsr carsf01;
064400150728                    endif;
064500150728                 endif;
064600150723              endif;
064700150723           enddo;
064800150723
064900150723          s01rcd    = s01nrr  ;
065000150723
065100150723         ENDSR  ;
065200150723       //--------------------------------------------------------------
065300150723       //?Preparazione stringa SQL
065400150723       //--------------------------------------------------------------
065500150723       BEGSR sr_prepsql;
065600150723       WrkStringaSql='select * from fnblp00f' +
065700150724       ' where (blpksc=' + %editc(v1cksc:'X') + ' or '      +
065800150724       ' blpccm=' + %editc(v1cksc:'X') +
065900150724       ') and blptfp=' + %editc(wtfp:'X') +
066000150723       ' and blpnfv=0' +
066100150723       ' order by  blptfp, blpnfv, blpdim';
066200150723       endsr;
066300150723       //--------------------------------------------------------------
066400150723       //?eseguo caricamento SFL
066500150723       //--------------------------------------------------------------
066600150723          BEGSR   carSF01    ;
066700150723          clear vs1opz  ;
066800150723
066900150723          exsr CarRECORD  ;
067000150723          vshaas=blpaas;
067100150723          vshlnp=blplnp;
067200150723          vshnrs=blpNRs;
067300150723          vshnsp=blpNsp;
067400150723          s01nrr=s01nrr+1   ;
067500150723          write  lsi1S01;
067600150723
067700150723          ENDSR         ;
067800150723       //--------------------------------------------------------------
067900150723       //?Caricamento record SFL
068000150723       //--------------------------------------------------------------
068100150723       BEGSR CarRecord;
068200150723       // data Immissione bolla
068300150723             clear vs1dim;
068400150723             datadmy=%date(blpdim:*iso);
068500150723             vs1dim=%dec(datadmy);
068600150723       // Data Spedizione
068700150723             clear vs1dsp;
068800150723             eval datadmy=%date((blpaas*10000+blpmgs):*iso);
068900150723             vs1dsp=%dec(datadmy);
069000150723       // linea di arrivo
069100150723             vs1lna=blplna;
069200150723             chain blplna azorg01l ;
069300150723             if orgde5<>*blanks;
069400150723                 vs1lnad=%subst(orgde5:1:10);
069500150723             else;
069600150723                 vs1lnad=%subst(orgdes:1:10);
069700150723             endif;
069800150723       // Destinatario
069900150723             vs1rsd=blprsd;
070000150723       ENDSR         ;
070100150723       //--------------------------------------------------------------
070200150723       //?controlli SFL
070300150723       //--------------------------------------------------------------
070400150723       BEGSR ContrS01;
070500150723
070600160308       *in32=*off ;
070700150723       s01nrr=1  ;
070800150723       readc lsi1s01    ;
070900150723
071000150723 0     dow not %eof                      ;
071100150723
071200160308            *in45=*off;
071300160308            *in28=*off;
071400150723       // Verifica l'opzione
071500150723          if vs1opz<>*blanks    ;
071600150723            *in19=*off;
071700150723          select;
071800150723          when vs1opz='1'                     ;
071900150724            chain (vshaas:vshlnp:vshnrs:vshnsp) fnblp01l ;
072000150914                 kaas=blpaas;
072100150914                 klnp=blplnp;
072200150914                 knrs=blpnrs;
072300150914                 knsp=blpnsp;
072400160308            ctrvid=*on ;
072500160308            exsr ctr_sfl;
072600150724            if *in28;
072700160308       //      eval *in19=*on;
072800160308               eval *in45=*on;
072900160308               eval *in32=*on;
073000150724            else;
073100160308               exsr calpgm;
073200150724            endif;
073300150724            chain (vshaas:vshlnp:vshnrs:vshnsp) fnblp01l ;
073400150723            if blpdcm>0;
073500150723               eval *in19=*on;
073600150723            endif;
073700150723          when vs1opz='5';
073800150723               exsr int_bolla;
073900150723          endsl;
074000150723               clear vs1opz;
074100150723          endif;
074200160308               update  lsi1s01  ;
074300160308
074400160308          *in32=*off ;
074500150723
074600150723       readc    lsi1s01    ;
074700150723 0     enddo         ;
074800150723
074900150723       ENDSR  ;
075000150723       //--------------------------------------------------------------
075100150723       //?Aggiornamento sfl
075200150723       //--------------------------------------------------------------
075300150723       BEGSR AGGIOSFL  ;
075400150723
075500150723       //Sflnxtchg=*on  ;
075600150723
075700150723       update  lsi1s01  ;
075800150723
075900150723       ENDSR   ;
076000150723      /end-free
076100150723     C*--------------------------------------------------------------*
076200150723     C* interroga bolla in partenza
076300150723     C*--------------------------------------------------------------*
076400150723     C     int_bolla     BEGSR
076500150723     C                   CLEAR                   PARA05
076600150723     C                   Z-ADD     VshAAS        P05AAS
076700150723     C                   Z-ADD     VshLNP        P05LNP
076800150723     C                   Z-ADD     VshNRS        P05NRS
076900150723     C                   Z-ADD     VshNSP        P05NSP
077000150723     C                   MOVEL     '1'           P05GIA
077100150723     C                   MOVEL     PARA05        KPJBU
077200150723     C                   CALL      'FNLS05R'
077300150723     C                   PARM                    KPJBA
077400150723     c                   endsr
077500970410     C*--------------------------------------------------------------*
077600130822     C* Decodifica i 2 riferimenti per impostare la spedizione
077700970410     C*--------------------------------------------------------------*
077800130822     C     decod_da_Rif  BEGSR
077900970410     C*
078000131016     c                   clear                   piu_bolle_Xrif    1
078100131016     c                   clear                   V2NOTE1
078200131016     c                   clear                   V2NOTE2
078300131016     C*
078400130822     c                   clear                   se_trovato        1
078500130822     c                   eval      se_trovato ='N'
078600130822     C*
078700130822     c                   if        v2rif32 <> *blank
078800131016     c     v2rif32       setll     fnblp32l
078900131016     c     v2rif32       reade     fnblp32l
079000131016     c                   dow       not %EoF(fnblp32l)
079100130822     c                   eval      se_trovato ='S'
079200131016     c     v2rif32       reade     fnblp32l
079300131016     c                   if        not %EoF(fnblp32l)
079400131016     c                   eval       piu_bolle_Xrif ='S'
079500131016     c                   end
079600130822     c                   end
079700130822     c                   end
079800130822      *
079900130822     c                   if        v2rif33 > 0 and
080000130822     c                             se_trovato ='N'
080100131016     c     v2rif33       setll     fnblp33l
080200131016     c     v2rif33       reade     fnblp33l
080300131016     c                   dow       not %Eof(fnblp33l)
080400130822     c                   eval      se_trovato ='S'
080500131016     c     v2rif33       reade     fnblp33l
080600131016     c                   if        not %EoF(fnblp33l)
080700131016     c                   eval       piu_bolle_Xrif ='S'
080800131016     c                   end
080900130822     c                   end
081000130822     c                   end
081100130822     C*
081200130822     C* Se lo ha trovato imposta imposta i campi della spedizione
081300131016     C*   ATTENZIONE potrebbero essercene più di una con lo stesso
081400131016     C* riferimento quindi segnala che ha preso l'ultima
081500130822     c                   IF        se_trovato ='S'
081600131016     c                               and v2cnsp = 0
081700130822     c                   eval      v2caas = blpaas
081800130822     c                   eval      v2clnp = blplnp
081900130822     c                   eval      v2cnrs = blpnrs
082000130822     c                   eval      v2cnsp = blpnsp
082100130822     c                   end
082200131016     C*   AVVISA SE HA trovato più bolle con lo stesso riferimento
082300131016     c                   IF         piu_bolle_Xrif ='S'
082400131016     c                   EVAL        V2NOTE1 = err(25)
082500131016     c                   EVAL        V2NOTE2 = err(26)
082600131016     c                   end
082700131016     C*
082800130822     C                   ENDSR
082900130822     C*--------------------------------------------------------------*
083000130822     C* CTR02: Controlli prima videata
083100130822     C*--------------------------------------------------------------*
083200130822     C     CTR02         BEGSR
083300130822     C*
083400130822     C*  pre- controllo bolla tramite rif.alfa o numerico
083500130822     c                   exsr      decod_da_Rif
083600130822     C*
083700970410     C* Controllo se immesso anno
083800970611     C     V2CAAS        IFEQ      0
083900970410     C                   SETON                                        2840
084000970611     C                   MOVEL     ERR(5)        $MSG
084100970410     C                   END
084200990610     C   28              GOTO      FINCT2
084300970410     C* Se anno digitato di 2 --> 4 cifre
084400970612     C                   MOVEL     V2CAAS        WSECOL            2 0
084500970410     C     WSECOL        IFEQ      0
084600970611     C     V2CAAS        IFGT      60
084700970611     C                   ADD       1900          V2CAAS
084800970410     C                   ELSE
084900970611     C                   ADD       2000          V2CAAS
085000970410     C                   END
085100970410     C                   END
085200970410     C* Controllo se immessa linea di partenza
085300970611     C     V2CLNP        IFEQ      0
085400970414     C                   SETON                                        2841
085500970611     C                   MOVEL     ERR(6)        $MSG
085600970410     C                   END
085700990610     C   28              GOTO      FINCT2
085800970410     C* Controllo esistenza lnp
085900970611     C                   Z-ADD     V2CLNP        ORGFIL
086000970410     C     ORGFIL        CHAIN     AZORG01L                           31
086100970611     C     *IN31         IFEQ      '1'
086200970410     C                   SETON                                        2841
086300970611     C                   MOVEL     ERR(7)        $MSG
086400970414     C                   END
086500990610     C   28              GOTO      FINCT2
086600970414     C* Controllo se lnp in £1
086700970611     C     V2CLNP        LOOKUP    £1                                     32
086800970414     C     *IN32         IFEQ      '0'
086900970414     C                   SETON                                        2841
087000970611     C                   MOVEL     ERR(8)        $MSG
087100970414     C                   END
087200990610     C   28              GOTO      FINCT2
087300970414     C* Controllo se immesso numero di spedizione
087400970611     C     V2CNSP        IFEQ      0
087500970414     C                   SETON                                        2843
087600970611     C                   MOVEL     ERR(9)        $MSG
087700970414     C                   END
087800990610     C   28              GOTO      FINCT2
087900970414     C* Controllo se esiste spedizione
088000970611     C                   Z-ADD     V2CAAS        KAAS
088100970611     C                   Z-ADD     V2CLNP        KLNP
088200970611     C                   Z-ADD     V2CNRS        KNRS
088300970611     C                   Z-ADD     V2CNSP        KNSP
088400970414     C     KBLP          CHAIN(N)  FNBLP01L                           31
088500970414     C     *IN31         IFEQ      '1'
088600970414     C                   SETON                                        2843
088700970611     C                   MOVEL     ERR(10)       $MSG
088800970616     C                   ELSE
088900970616     C* Controllo che la spedizione sia del cliente selezionato
089000970616     C     BLPCCM        IFNE      V2CKSC
089100970616     C     BLPKSC        ANDNE     V2CKSC
089200970616     C                   SETON                                        2843
089300980202     C                   MOVEL     ERR(17)       $MSG
089400970414     C                   END
089500970616     C                   END
089600990610     C   28              GOTO      FINCT2
089700970414     C* Controllo se bolla mamma
089800150728     C*                  Z-ADD     V2CAAS        KAAP
089900150728     C*                  Z-ADD     V2CLNP        KLNP
090000150728     C*                  Z-ADD     V2CNRS        KNRP
090100150728     C*                  Z-ADD     V2CNSP        KNSP
090200150728     C*    KBLP          CHAIN     FNLBL02L                           31
090300970414     C* Controllo se bolla figlia
090400150728     C*                  Z-ADD     V2CAAS        KAAP
090500150728     C*                  Z-ADD     V2CLNP        KLNP
090600150728     C*                  Z-ADD     V2CNRS        KNRP
090700150728     C*                  Z-ADD     V2CNSP        KNSP
090800150728     C*    KBLP          CHAIN     FNLBL01L                           31
090900150728     C*    *IN31         IFEQ      '0'
091000150728     C*                  SETON                                        284344
091100150728     C*                  MOVEL     ERR(12)       $MSG
091200150728     C*                  END
091300150728     C*  28              GOTO      FINCT2
091400980204     C* Controllo che non sia stata data per consegnata
091500150728     C*    BLPDCM        IFNE      0
091600150728     C*    BLPCCA        ANDNE     '6'
091700150728     C*    BLPDCM        ORNE      0
091800150728     C*    BLPCCA        ANDNE     '2'
091900150728     C*                  SETON                                        2843
092000150728     C*                  MOVEL     ERR(22)       $MSG
092100150728     C*                  END
092200150728     C*  28              GOTO      FINCT2
092300980204     C* Controllo che abbia il dettaglio segnacolli
092400150728     C*                  Z-ADD     V2CAAS        KAAS
092500150728     C*+                 Z-ADD     V2CLNP        KLNP
092600150728     C**                 Z-ADD     V2CNRS        KNRS
092700150728     C*                  Z-ADD     V2CNSP        KNSP
092800150728     C*    KBLT          CHAIN     FNBLT01L                           31
092900150728     C*    *IN31         IFEQ      '1'
093000150728     C*                  SETON                                        2843
093100150728     C*                  MOVEL     ERR(16)       $MSG
093200150728     C*                  END
093300150728     c                   exsr      ctr_uniS
093400150728     C   28              GOTO      FINCT2
093500980202     C* Controllo se richiesto '?'
093600980202     C     '?'           SCAN      V2CSTS                                 32
093700980202     C     *IN32         IFEQ      '1'
093800980202     C                   MOVE      LAB(XY)       DI1LAB
093900980202     C                   MOVEL     FNLSI1        KPJBU
094000980203     C                   CALL      'FNLSI1R5'
094100980202     C                   PARM                    KPJBA
094200980204     C                   MOVEL     KPJBU         FNLSI1
094300980204     C                   MOVEL     DI1CEV        V2CCOD
094400980202     C                   MOVEL     DI1STS        V2CSTS
094500980202     C                   END
094600980202     C* evento obbligatoria
094700980202     C     V2CCOD        IFEQ      *BLANKS
094800980202     C     V2CSTS        ANDEQ     *BLANKS
094900970721     C                   SETON                                        2844
095000980202     C                   MOVEL     ERR(21)       $MSG
095100970721     C                   END
095200990610     C   28              GOTO      FINCT2
095300980202     C* evento esistente
095400980202     C                   MOVEL     V2CSTS        WEVE              6
095500980202     C                   MOVE      V2CCOD        WEVE
095600030116     C                   Z-ADD     1             YY                5 0
095700980202     C     LAB(XY)       IFEQ      'S'
095800980202     C     WEVE          LOOKUP    ELB(YY)                                32
095900980202     C                   MOVEL     DLB(YY)       DSEM
096000980202     C                   ELSE
096100980202     C     WEVE          LOOKUP    EVG(YY)                                32
096200980202     C                   MOVEL     DVG(YY)       DSEM
096300980203     C                   MOVEL     §EMDES        V2DCOD
096400980202     C                   END
096500980204     C     *IN32         IFEQ      '0'
096600980202     C                   SETON                                        2844
096700980202     C                   MOVEL     ERR(19)       $MSG
096800980202     C                   END
096900990610     C   28              GOTO      FINCT2
097000980203     C* Se bolla in assegnato no IDD Totale definitivo
097100150728     C*                  Z-ADD     1             X
097200150728     C*    BLPCBO        LOOKUP    C3A(X)                                 32
097300150728     C*                  MOVEL     TPO(X)        WTPO              1
097400150728     C*    *IN32         IFEQ      '1'
097500150728     C*    WTPO          ANDEQ     'A'
097600150728     C*    §EMASS        ANDNE     'S'
097700150728     C*                  SETON                                        2843
097800150728     C*                  MOVEL     ERR(20)       $MSG
097900150728     C*                  END
098000150728     C*  28              GOTO      FINCT2
098100970414     C* Controllo che non sia stata data per partita
098200150728     C*    BLPDT1        IFNE      0
098300150728     C*    §EMTRA        ANDNE     'S'
098400150728     C*                  SETON                                        2843
098500150728     C*                  MOVEL     ERR(13)       $MSG
098600150728     C*                  END
098700150728     C*  28              GOTO      FINCT2
098800970414     C* Controllo che non sia stata trasmessa in sede
098900150728     C*    BLPDT2        IFNE      0
099000150728     C*    §EMTRA        ANDNE     'S'
099100150728     C*                  SETON                                        2843
099200150728     C*                  MOVEL     ERR(14)       $MSG
099300150728     C*                  END
099400150728     C*  28              GOTO      FINCT2
099500970414     C* Controllo che non sia stata trasmessa al cliente
099600150728     C*    BLPDT3        IFNE      0
099700150728     C*    §EMTRA        ANDNE     'S'
099800150728     C*                  SETON                                        2843
099900150728     C*                  MOVEL     ERR(15)       $MSG
100000150728     C*                  END
100100150728     C*  28              GOTO      FINCT2
100200990610      * Controllo C.A.
100300150728     C*    §EMCAC        IFEQ      'S'
100400040715      *
100500150728     c*                  clear                   fidn12ds
100600150728     c*                  eval      i12aas = V2CAAS
100700150728     c*                  eval      i12lnp = V2CLNP
100800150728     c*                  eval      i12nrs = V2CNRS
100900150728     c*                  eval      i12nsp = V2CNSP
101000150728     c*                  eval      i12fel = 'E'
101100150728     c*                  eval      i12fan = 'E'
101200150728     c*                  eval      i12fch = 'E'
101300150728     c*                  call      'FIDN12R'
101400150728     c*                  parm                    fidn12ds
101500040715      **
101600040715      * se c'è almeno una C.A. aperta
101700150728     c*    o12nca        ifgt      0
101800150728     C*                  SETON                                        284331
101900150728     C*                  MOVEL     ERR(24)       $MSG
102000150728     c*                  end
102100040715      **
102200150728     C*                  ENDIF
102300040715      **
102400160307     c                   eval      ctrvid=*on
102500150728     c                   exsr      ctr_uniE
102600990610     C   28              GOTO      FINCT2
102700040715      **
102800980204     C* Controllo se l'evento è già stato inviato
102900980204     C                   Z-ADD     V2CAAS        KAA2
103000980204     C                   MOVEL     §EMCOD        KCEV
103100980204     C                   MOVEL     ' '           WDAFOR            1
103200990810     C     KEVB          CHAIN     FNEVB04L                           31
103300980204     C* Evento già immesso
103400980204     C     *IN31         IFEQ      '0'
103500980204     C* se non è possibile forzare do errore
103600980204     C     §EMFOR        IFNE      'S'
103700980204     C                   SETON                                        2843
103800980204     C                   MOVEL     ERR(23)       $MSG
103900980204     C                   ELSE
104000980204     C                   MOVEL     'F'           WDAFOR
104100980204     C                   END
104200980204     C                   END
104300990610     C   28              GOTO      FINCT2
104400970414     C*
104500970612     C     FINCT2        ENDSR
104600150724     C*--------------------------------------------------------------*
104700150728     C*    Controlli per il caricamento e gestione sfl
104800150724     C*--------------------------------------------------------------*
104900150728     C     ctr_sfl       BEGSR
105000150724     c                   clear                   $msg
105100150724     c                   setoff                                       28
105200150728     c                   exsr      ctr_uniS
105300150728     c                   if        *in28
105400150728     c                   leavesr
105500150728     c                   endif
105600150728     c                   exsr      ctr_uniE
105700150728     c                   if        *in28
105800150728     c                   leavesr
105900150728     c                   endif
106000150724     c                   clear                   wsispu            1
106100150724     c* controllo che la spedizione non abbia spunte
106200150728     c     kblt          setll     fnblt01l
106300150728     c     kblt          reade     fnblt01l                               99
106400150724    2c                   dow       not *in99
106500150724     c
106600150724     c* Cerco una qualsiasi spunta
106700150724     c     kbrv          chain     fnbrv07l
106800150724    3c                   if        %found(fnbrv07l)
106900150724     c                   eval      wsispu='S'
107000150724     c                   seton                                        99
107100150724   x3c                   else
107200150728     c     kblt          reade     fnblt01l                               99
107300150724    3c                   endif
107400150724    2c                   enddo
107500150724     c*
107600150724    2c                   if        wsispu='S'
107700150724     c                   seton                                        28
107800150724     C                   MOVEL     ERR(29)       $MSG
107900150724     c                   leavesr
108000150724     c                   endif
108100150724     c
108200150724     c
108300150724     c                   endsr
108400150728     C*--------------------------------------------------------------*
108500150728     C*    Controlli unici per spedizione
108600150728     C*--------------------------------------------------------------*
108700150728     C     ctr_uniS      BEGSR
108800150728     C* Controllo se bolla figlia
108900150728     C     KBLP          CHAIN     FNLBL01L
109000150728     C                   if        %found(fnlbl01l)
109100150728     C                   SETON                                        28
109200150728     C                   SETON                                          4344
109300150728     C                   MOVEL     ERR(12)       $MSG
109400150728     c                   leavesr
109500150728     C                   END
109600150728     C* Controllo che non sia stata data per consegnata
109700150728     C     BLPDCM        IFNE      0
109800150728     C     BLPCCA        ANDNE     '6'
109900150728     C     BLPDCM        ORNE      0
110000150728     C     BLPCCA        ANDNE     '2'
110100150728     C                   SETON                                        28
110200150728     C                   SETON                                        43
110300150728     C                   MOVEL     ERR(22)       $MSG
110400150728     c                   leavesr
110500150728     C                   END
110600150728     C* Controllo che abbia il dettaglio segnacolli
110700150728     c     kblt          setll     fnblt01l
110800150728     C                   IF        not %equal(fnblt01l)
110900150728     C                   SETON                                        28
111000150728     C                   SETON                                        43
111100150728     C                   MOVEL     ERR(16)       $MSG
111200150728     c                   leavesr
111300150728     C                   END
111400150728     c                   endsr
111500150728     C*--------------------------------------------------------------*
111600150728     C*    Controlli unici legati all'evento
111700150728     C*--------------------------------------------------------------*
111800150728     C     ctr_uniE      BEGSR
111900150728     C* Se bolla in assegnato no IDD
112000160307     c* Solo se vengo da controlli videata
112100160307     c                   if        ctrvid=*on
112200160307     C                   Z-ADD     1             X
112300160307     C     BLPCBO        LOOKUP    C3A(X)                                 32
112400160307     C                   MOVEL     TPO(X)        WTPO              1
112500160307     C     *IN32         IFEQ      '1'
112600160307     C     WTPO          ANDEQ     'A'
112700160307     C     §EMASS        ANDNE     'S'
112800160308     c                   eval      ktrc='M'
112900160308     c     kar4          chain     fiar401l
113000160308     c                   if        %found(fiar401l)
113100160308     c                   eval      dsbl4m=ar4not
113200160308     c     korm          chain     fnorm01l
113300160308     c                   if        %found(fnorm01l) and ormfao<>999
113400160307     C                   SETON                                        28
113500160307     C                   SETON                                        43
113600160307     C                   MOVEL     ERR(20)       $MSG
113700160307     c                   leavesr
113800160308     c                   endif
113900160308     c                   endif
114000160307     C                   END
114100160307     c                   endif
114200150728     C* Controllo che non sia stata data per partita
114300150728     C     BLPDT1        IFNE      0
114400150728     C     §EMTRA        ANDNE     'S'
114500150728     C                   SETON                                        2843
114600150728     C                   MOVEL     ERR(13)       $MSG
114700150728     c                   leavesr
114800150728     C                   END
114900150728     C* Controllo che non sia stata trasmessa in sede
115000150728     C     BLPDT2        IFNE      0
115100150728     C     §EMTRA        ANDNE     'S'
115200150728     C                   SETON                                        2843
115300150728     C                   MOVEL     ERR(14)       $MSG
115400150728     c                   leavesr
115500150728     C                   END
115600150728     C* Controllo che non sia stata trasmessa al cliente
115700150728     C     BLPDT3        IFNE      0
115800150728     C     §EMTRA        ANDNE     'S'
115900150728     C                   SETON                                        2843
116000150728     C                   MOVEL     ERR(15)       $MSG
116100150728     c                   leavesr
116200150728     C                   END
116300150728      * Controllo C.A.
116400150728      *
116500150728     C     §EMCAC        IFEQ      'S'
116600150728     c                   clear                   fidn12ds
116700150728     c                   eval      i12aas = blpaAS
116800150728     c                   eval      i12lnp = blpLNP
116900150728     c                   eval      i12nrs = blpNRS
117000150728     c                   eval      i12nsp = blpNSP
117100150728     c                   eval      i12fel = 'E'
117200150728     c                   eval      i12fan = 'E'
117300150728     c                   eval      i12fch = 'E'
117400150728     c                   call      'FIDN12R'
117500150728     c                   parm                    fidn12ds
117600150728      **
117700150728      * se c'è almeno una C.A. aperta
117800150728     c     o12nca        ifgt      0
117900150728     C                   SETON                                        28
118000150728     C                   SETON                                          4331
118100150728     C                   MOVEL     ERR(24)       $MSG
118200150728     c                   leavesr
118300150728     c                   end
118400150728     C                   ENDIF
118500150728     c                   endsr
118600970612     C*--------------------------------------------------------------*
118700970612     C* CALPGM: Eseguo Call a pgm
118800970612     C*--------------------------------------------------------------*
118900970612     C     CALPGM        BEGSR
119000970612     C*
119100970612     C*  Imposto DS per richiamo altri pgm
119200970612     C                   CLEAR                   FNLSI1
119300150723     c                   if        wtpvid='S'
119400150723     C                   clear                   DI1FTX
119500150723     C                   MOVEL     V1CKSC        DI1KSC
119600150723     C                   MOVEL     VhdKSC        DI1DEC
119700150723     C                   MOVEL     VshLNP        DI1LNP
119800150723     C                   MOVEL     VshNRS        DI1NRS
119900150723     C                   MOVEL     VshNSP        DI1NSP
120000150723     C                   MOVEL     VshAAS        DI1AAS
120100150723     C                   MOVEL     VshAAS        DI1DSP
120200150723     C                   MOVE      BLPMGS        DI1DSP
120300150723     C                   MOVE      LAB(XY)       DI1LAB
120400150723     C                   MOVE      wsts          DI1STS
120500150723     C                   MOVE      wcev          DI1CEV
120600160209     C**                 MOVEL     wsts          WEVE              6
120700160209     C**                 MOVE      wcev          WEVE
120800150723
120900160209     C***                Z-ADD     1             YY                5 0
121000160209     C**   LAB(XY)       IFEQ      'S'
121100160209     C***  WEVE          LOOKUP    ELB(YY)                                32
121200160209     C***                MOVEL     DLB(YY)       DSEM
121300160209     C***                ELSE
121400160209     C***  WEVE          LOOKUP    EVG(YY)                                32
121500160209     C***                MOVEL     DVG(YY)       DSEM
121600160209     C***                END
121700150723
121800150723     C                   MOVE      §EMDES        DI1DEV
121900150723     C                   MOVE      §EMDES        DI1DEV
122000150723     C                   MOVE      §EMCOD        DI1C2A
122100150723     C                   MOVE      §EMCCA        DI1CCA
122200150723     C                   MOVE      §EMBLP        DI1BLP
122300150723     C                   MOVE      §EMBLT        DI1BLT
122400150723     C                   MOVE      §EMFOR        DI1FRZ
122500150723     C                   MOVE      §EMTOT        DI1TOT
122600150723     C                   MOVE      BLPNCL        DI1NCL
122700150723     c                   else
122800980203     C                   MOVEL     V2CFTX        DI1FTX
122900980203     C                   MOVEL     V2CKSC        DI1KSC
123000970612     C                   MOVEL     V2DKSC        DI1DEC
123100970612     C                   MOVEL     V2CLNP        DI1LNP
123200970612     C                   MOVEL     V2CNRS        DI1NRS
123300970612     C                   MOVEL     V2CNSP        DI1NSP
123400970612     C                   MOVEL     V2CAAS        DI1AAS
123500970612     C                   MOVEL     V2CAAS        DI1DSP
123600970612     C                   MOVE      BLPMGS        DI1DSP
123700970616     C                   MOVE      BLPMGS        DI1DSP
123800971007     C                   MOVE      LAB(XY)       DI1LAB
123900150723     C                   MOVE      V2CSTS        DI1STS
124000150723     C                   MOVE      V2CCOD        DI1CEV
124100980202     C                   MOVE      §EMDES        DI1DEV
124200980203     C                   MOVE      §EMDES        DI1DEV
124300980203     C                   MOVE      §EMCOD        DI1C2A
124400980203     C                   MOVE      §EMCCA        DI1CCA
124500980203     C                   MOVE      §EMBLP        DI1BLP
124600980203     C                   MOVE      §EMBLT        DI1BLT
124700980203     C                   MOVE      §EMFOR        DI1FRZ
124800980203     C                   MOVE      §EMTOT        DI1TOT
124900980204     C                   MOVE      WDAFOR        DI1FOR
125000980204     C                   MOVE      BLPNCL        DI1NCL
125100150723     c                   endif
125200970612     C                   MOVEL     FNLSI1        KPJBU
125300980202     C* Richiamo pgm gestione dettaglio segnacolli
125400980202     C     §EMTOT        IFNE      'S'
125500980202     C                   CALL      'FNLSI1R2'
125600980202     C                   PARM                    KPJBA
125700980202     C                   MOVEL     KPJBU         FNLSI1
125800980202     C                   END
125900980202     C* Se ho digitato F3 esco
126000980202     C     DI1CMD        IFEQ      '03'
126100980202     C                   MOVEL     'S'           WFINE
126200980202     C                   GOTO      FINRIC
126300980202     C                   END
126400980204     C* Se ho digitato F12 azzero campo scelta
126500980204     C     DI1CMD        IFEQ      '12'
126600980204     C                   GOTO      FINRIC
126700980204     C                   END
126800980202     C* Richiamo pgm gestione terza videata: SPEDIZIONE
126900970612     C                   CALL      'FNLSI1R1'
127000970612     C                   PARM                    KPJBA
127100970617     C                   MOVEL     KPJBU         FNLSI1
127200970612     C*
127300970612     C* Se ho digitato F3 esco
127400970612     C                   SELECT
127500970612     C     DI1CMD        WHENEQ    '03'
127600970619     C                   MOVEL     'S'           WFINE
127700970619     C* Controllo se devo richiamare TRTC81
127800970619     C     WRIC81        IFEQ      'S'
127900970619     C                   EXSR      CALL81
128000970619     C                   END
128100970612     C* Se ho digitato F12 azzero campo scelta
128200970612     C     DI1CMD        WHENEQ    '12'
128300970616     C* CONFERMA
128400970616     C     DI1CMD        WHENEQ    '06'
128500970617     C                   MOVEL     'S'           WRIC81
128600970616     C* Aggiorno BLP/BLT
128700980202     C     §EMBLP        IFEQ      'S'
128800970612     C                   CALL      'FNLSI1R3'
128900970617     C                   PARM                    KPJBA
129000970616     C                   END
129100080421      * l'EDIVAC viene sostituito con il TIVGD Download standard
129200080421     C* Scrivo EDVAC --> TIVGD00F
129300970612     C                   CALL      'FNLSI1R4'
129400970612     C                   PARM                    KPJBA
129500970616     C                   ENDSL
129600970612     C*
129700980202     C     FINRIC        ENDSR
129800970617     C*--------------------------------------------------------------*
129900980126     C* CALL81: PER E.D.I. RICHIAMO PGM TRTC81C
130000970617     C*--------------------------------------------------------------*
130100970617     C     CALL81        BEGSR
130200970617     C*
130300980126      * CALL A TRTC81C PER E.D.I.
130400980126     C     §PTFTT        IFNE      'S'
130500061124     C     §PTFTT        ANDNE     'P'
130600980126     C                   MOVEL     'S'           WCAL81            1
130700080421      *
130800080421      *  Scrive IFTSTA x EDI:
130900080421      * ? il TRTC81C  attiva giro di trasm con SNDIFTSTA  ?
131000080421     C                   MOVEL     *all'0'       Elab_Cliente
131100080421     C                   MOVE      CKS(XX)       Elab_Cliente
131200080421     c                   eval      Tipo_Scarico = CSD(XX)
131300080421      *
131400080421     c                   eval      Tipo_File = 'VC'
131500110630     C                   CALL      'TRTCT81C'
131600080421     c                   parm                    Elab_Cliente      8
131700080422     c                   parm                    Tipo_File         2
131800080421     c                   parm                    Tipo_Scarico      2
131900980126      *
132000980126     C                   END
132100980126      *
132200970617     C                   ENDSR
132300970410     C*--------------------------------------------------------------*
132400970410     C* *INZSR: Operazioni iniziali
132500970410     C*--------------------------------------------------------------*
132600970410     C     *INZSR        BEGSR
132700970410     C*
132800970410     C     *ENTRY        PLIST
132900970410     C                   PARM                    KPJBA
133000990223     C* Richiamo xparut
133100970410     C                   Z-ADD     1             CODUT
133200970410     C                   CALL      'X§PARUT'
133300970410     C                   PARM                    UTEDSE
133400970410     C                   MOVEL     REC80         CNCR80
133500970410     C                   MOVEL     RAGUT         RSUT
133600030924      *
133700030924      *  In base all'utente prende il P.O.
133800030924     C                   CLEAR                   Tibs36Ds
133900030924     c                   EVAL      I36ute = Knmus
134000030924     c                   EVAL      I36Tla = 'L'
134100030924     C                   CALL      'TIBS36R'
134200030924     C                   PARM                    Tibs36Ds
134300030924      *
134400970612     C*--- RICERCA CAPOCONTI------------------------------------------*
134500030116     C                   DO        50            X                 5 0
134600970612     C                   MOVE      TCU(X)        TCUDS
134700970612     C     F56           IFEQ      'CG'
134800970612     C     F4            ANDEQ     '1'
134900970612     C     F3            ANDEQ     '0'
135000970612     C                   Z-ADD     KCU(X)        CLIITA            4 0
135100970612     C                   END
135200970612     C                   END
135300970612     C*--- RICERCA CAPOCONTI------------------------------------------*
135400970410      *  Definizione chiavi
135500970410     C     KBLP          KLIST
135600970410     C                   KFLD                    KAAS
135700970410     C                   KFLD                    KLNP
135800970410     C                   KFLD                    KNRS
135900970410     C                   KFLD                    KNSP
136000970410     C     KBLT          KLIST
136100970410     C                   KFLD                    KAAS
136200970410     C                   KFLD                    KLNP
136300970410     C                   KFLD                    KNRS
136400970410     C                   KFLD                    KNSP
136500970410     C     KLBL1         KLIST
136600970410     C                   KFLD                    KAAP
136700970410     C                   KFLD                    KLPP
136800970410     C                   KFLD                    KNRP
136900970410     C                   KFLD                    KNSP
137000970410     C     KLBL2         KLIST
137100970410     C                   KFLD                    KAAN
137200970410     C                   KFLD                    KLPN
137300970410     C                   KFLD                    KNRN
137400970410     C                   KFLD                    KNSN
137500970410     C     KTAB1         KLIST
137600970410     C                   KFLD                    KKUT
137700970410     C                   KFLD                    KCOD
137800970410     C     KTAB2         KLIST
137900970410     C                   KFLD                    KKUT
138000970410     C                   KFLD                    KCOD
138100970410     C                   KFLD                    KKEY
138200970612     C     KACO          KLIST
138300970612     C                   KFLD                    KKUT
138400970612     C                   KFLD                    KKCC
138500970612     C                   KFLD                    KKSC
138600980204     C     KEVB          KLIST
138700980204     C                   KFLD                    KAA2
138800980204     C                   KFLD                    KLNP
138900980204     C                   KFLD                    KNRS
139000980204     C                   KFLD                    KNSP
139100980204     C                   KFLD                    KCEV
139200080918     C     KTBE1         KLIST
139300080918     C                   KFLD                    TBECOD
139400080918     C                   KFLD                    TBEKE1
139500150415     C     Kedtab        KLIST
139600150415     C                   KFLD                    tabCOD
139700150415     C                   KFLD                    tabKEY
139800150724     C     KBrv          KLIST
139900150724     C                   KFLD                    bltfls
140000150724     C                   KFLD                    bltlna
140100150724     C                   KFLD                    bltNRS
140200150724     C                   KFLD                    bltnsc
140300160308     C     Kar4          KLIST
140400160308     C                   KFLD                    KAAS
140500160308     C                   KFLD                    KLNP
140600160308     C                   KFLD                    KNRS
140700160308     C                   KFLD                    KNSP
140800160308     C                   KFLD                    Ktrc
140900160308     C     Korm          KLIST
141000160308     C                   KFLD                    §B4POE
141100160308     C                   KFLD                    §B4NSR
141200160308     C                   KFLD                    §B4NOR
141300160308     C                   KFLD                    §B4NRV
141400970410      *  Definizione variabili
141500980204     C     *LIKE         DEFINE    EVBAAS        KAA2
141600980204     C     *LIKE         DEFINE    EVBCEV        KCEV
141700970410     C     *LIKE         DEFINE    BLPAAS        KAAS
141800970410     C     *LIKE         DEFINE    BLPLNP        KLNP
141900970410     C     *LIKE         DEFINE    BLPNRS        KNRS
142000970410     C     *LIKE         DEFINE    BLPNSP        KNSP
142100970410     C     *LIKE         DEFINE    LBLAAP        KAAP
142200970410     C     *LIKE         DEFINE    LBLLPP        KLPP
142300970410     C     *LIKE         DEFINE    LBLNRP        KNRP
142400970410     C     *LIKE         DEFINE    LBLAAN        KAAN
142500970410     C     *LIKE         DEFINE    LBLLPN        KLPN
142600970410     C     *LIKE         DEFINE    LBLNRN        KNRN
142700970410     C     *LIKE         DEFINE    LBLNSN        KNSN
142800970410     C     *LIKE         DEFINE    TBLKUT        KKUT
142900970410     C     *LIKE         DEFINE    TBLCOD        KCOD
143000970410     C     *LIKE         DEFINE    TBLKEY        KKEY
143100970611     C     *LIKE         DEFINE    TABCOD        KCODE
143200970612     C     *LIKE         DEFINE    ACOKCC        KKCC
143300970612     C     *LIKE         DEFINE    ACOKSC        KKSC
143400970410      *
143500970410      *  Reperisco la data del giorno
143600970410     C                   TIME                    WHHDAT           14 0
143700970414     C                   MOVE      WHHDAT        G02DAT
143800970414     C                   MOVEL     *BLANK        G02ERR
143900970414     C                   CALL      'XSRDA8'
144000970414     C                   PARM                    WLBDA8
144100970414     C                   Z-ADD     G02INV        OGGI              8 0
144200970611     C*
144300970611     C* Carico dati DS partner in schiera
144400970612     C                   OPEN      EDTAB01L
144500150415     C                   Z-ADD     0             X
144600970611     C                   MOVEL     'PT'          KCODE
144700970616     C     KCODE         CHAIN     EDTAB01L                           30
144800970611     C     *IN30         DOWEQ     '0'
144900970611     C     TABFLG        IFEQ      ' '
145000970611     C                   ADD       1             X
145100970611     C                   MOVEL     TABUNI        DSPT
145200150415     C                   MOVEL     TABKEY        KEYPT(X)
145300970611     C                   MOVEL     §PTKSC        KSC(X)
145400970616     C                   MOVEL     §PTLNP        LNP(X)
145500970616     C                   MOVEL     §PTNRS        NRS(X)
145600970611     C                   END
145700970616     C     KCODE         READE     EDTAB01L                               30
145800970611     C                   END
145900970616     C*
146000150415     C* x evitare che a causa di un disallineamento imprevisto
146100150415     C*  la PU possa essere disallineata alla PT  eseguo un caricamento
146200150415     C*  differente.
146300150415     c                   clear                   lab
146400150415     c                   do        x             pux               5 0
146500150415     C                   MOVEL     'PU'          tabCOD
146600150415     C                   MOVEL     KEYPT(pux)    tabKEY
146700150415     C     Kedtab        CHAIN     EDTAB01L
146800150415     c                   if        %Found(edtab01l) and tabflg =' '
146900150415      *  Lista Labarta
147000150415     C                   MOVEL     TABUNI        DSPU
147100150415     C                   MOVEL     §PULAB        LAB(pux)
147200150415     c                   end
147300150415     c                   enddo
147400150415      *
147500150415      **** vecchio caricamento
147600150415     C****               Z-ADD     0             X
147700150415     C****               MOVEL     'PU'          KCODE
147800150415     C**** KCODE         CHAIN     EDTAB01L                           30
147900150415     C**** *IN30         DOWEQ     '0'
148000150415     C**** TABFLG        IFEQ      ' '
148100150415     C****               ADD       1             X
148200150415     C****               MOVEL     TABUNI        DSPU
148300150415     C****               MOVEL     §PULAB        LAB(X)
148400150415     C****               END
148500150415     C**** KCODE         READE     EDTAB01L                               30
148600150415     C****               END
148700150415      *
148800150415     C*  Carico in schiera codici collegati - CL -
148900150415     C                   Z-ADD     0             X
149000150415     C                   MOVEL     'CL'          KCODE
149100150415     C     KCODE         CHAIN     EDTAB01L                           30
149200150415     C     *IN30         DOWEQ     '0'
149300150415     C     TABFLG        IFEQ      ' '
149400150415     C                   ADD       1             X
149500150415     C                   MOVEL     TABUNI        DSCL
149600150415     C                   MOVEL     §CLKSC        clKSC(X)
149700150415     C                   MOVEL     §CLLNP        clLNP(X)
149800150415     C                   END
149900150415     C     KCODE         READE     EDTAB01L                               30
150000150415     C                   END
150100980202     C*
150200980202     C* Carico in schiera tabella eventi manuali
150300150723     c                   clear                   wcev
150400150723     c                   clear                   wsts
150500980202     C                   Z-ADD     0             X
150600030116     C                   Z-ADD     0             X1                5 0
150700980202     C                   MOVEL     'EM'          KCODE
150800980202     C     KCODE         CHAIN     EDTAB01L                           30
150900980202     C     *IN30         DOWEQ     '0'
151000980202     C     TABFLG        IFEQ      ' '
151100980202     C                   MOVEL     TABUNI        DSEM
151200980202     C     §EMLAB        IFEQ      'S'
151300980202     C                   ADD       1             X
151400980204     C                   MOVEL     TABKEY        ELB(X)
151500980202     C                   MOVEL     TABUNI        DLB(X)
151600980202     C                   END
151700980202     C     §EMVGL        IFEQ      'S'
151800980202     C                   ADD       1             X1
151900980204     C                   MOVEL     TABKEY        EVG(X1)
152000980202     C                   MOVEL     TABUNI        DVG(X1)
152100980202     C                   END
152200150723     c* memorizzo il codice dell'evento corrispondente alla chiusura bolla
152300150723     c* per merce mai affidata
152400150723     c                   if        §emmai='S'
152500150723     c                   movel     tabkey        wsts
152600150723     c                   eval      wcev=%subst(tabkey:4:3)
152700160209     c                   eval      s_dsem=dsem
152800150723     c                   endif
152900980202     C     KCODE         READE     EDTAB01L                               30
153000980202     C                   END
153100980202     C                   END
153200970611     C* close EDTAB
153300970611     C                   CLOSE     EDTAB01L
153400970611     C*
153500970410     C* Eseguo caricamento tabella 3K
153600970612     C                   OPEN      TABEL00F
153700970410     C                   Z-ADD     0             X
153800970410     C                   Z-ADD     1             KKUT
153900970410     C                   MOVEL     '3K'          KCOD
154000970410     C     KTAB1         CHAIN     TABEL                              30
154100970410     C     *IN30         DOWEQ     '0'
154200030213     C     X             ANDLT     9990
154300970410     C     TBLFLG        IFEQ      *BLANKS
154400970410     C                   ADD       1             X
154500970617     C                   MOVEL     TBLUNI        DS3K
154600970617     C                   MOVEL     TBLKEY        C3K(X)
154700970617     C                   MOVEL     §3KCKS        CKS(X)
154800010725     C                   MOVEL     §3KCSD        CSD(X)
154900970410     C                   END
155000970410     C     KTAB1         READE     TABEL                                  30
155100970410     C                   END
155200970715     C* Eseguo caricamento tabella 3A
155300970721     C                   Z-ADD     0             X
155400970715     C                   MOVEL     '3A'          KCOD
155500970715     C     KTAB1         CHAIN     TABEL                              30
155600970715     C     *IN30         DOWEQ     '0'
155700970721     C     X             ANDLT     500
155800970715     C     TBLFLG        IFEQ      *BLANKS
155900970715     C                   ADD       1             X
156000970715     C                   MOVEL     TBLUNI        DS3A
156100970715     C                   MOVEL     TBLKEY        C3A(X)
156200970715     C                   MOVEL     §3ATB1        TPO(X)
156300970715     C                   END
156400970715     C     KTAB1         READE     TABEL                                  30
156500970715     C                   END
156600970701     C*
156700970701     C* Eseguo caricamento da tabella 7J codici pistole fittizie
156800970701     C                   MOVEA     *HIVAL        C7J
156900970701     C                   Z-ADD     0             X
157000970701     C                   Z-ADD     1             KKUT
157100970701     C                   MOVEL     '7J'          KCOD
157200970701     C     KTAB1         CHAIN     TABEL                              30
157300970701     C     *IN30         DOWEQ     '0'
157400970721     C     X             ANDLT     500
157500970701     C     TBLFLG        IFEQ      *BLANKS
157600970701     C                   MOVEL     TBLUNI        DS7J
157700970912     C     §7JRPS        IFEQ      'A'
157800970701     C                   ADD       1             X
157900970701     C                   MOVEL     TBLKEY        C7J(X)
158000970701     C                   END
158100970701     C                   END
158200970701     C     KTAB1         READE     TABEL                                  30
158300970701     C                   END
158400150728
158500150728     C                   MOVEL     '3I'          Kcod
158600150728     C                   MOVEL     '1       '    kKEY
158700150728     c                   clear                   ds3i
158800150728     C     KTAB2         CHAIN     TABEL00F
158900150728     c                   if        %found(tabel00f)
159000150728     C                   MOVEL     TBLUNI        DS3I
159100150728     c                   endif
159200970410     C* Eseguo caricamento £1
159300020506     c                   clear                   trul06ds
159400020506     c                   eval      d06cod = '£1'
159500030924     c                   movel     o36pou        d06key
159600020506     c                   movel     trul06ds      kpjbu
159700020506     c                   call      'TRUL06R'
159800020506     c                   parm                    kpjba
159900020506     c                   movel     kpjbu         trul06ds
160000020506     c                   movea     lin           £1
160100150724     c* terminal di partenza del p.o. utente
160200150724     c                   clear                   fnlv55ds
160300150724     C                   MOVEL     'P'           D55TPT
160400150724     C                   MOVEL     simpou        D55LIN
160500150724     C                   z-add     oggi          D55drf
160600150724     C                   CALL      'FNLV55R'
160700150724     C                   PARM                    fnlv55ds
160800150724     c                   z-add     d55tfp        wtfp
160900020912      *
161000971008     C* Imposto a 'N' flag richiamo TRTC81C
161100971008     C                   MOVEL     'N'           WCAL81            1
161200970410     C* Eseguo close del file
161300970410     C                   CLOSE     TABEL00F
161400970410     C*
161500970410     C                   ENDSR
161600080930      *------------------------------------------------------------------------*
161700080930      * Routine di controllo x eventuali Errori
161800080930      *------------------------------------------------------------------------*
161900080930     C     *PSSR         BEGSR
162000080930      *
162100080930      * Comunque eseguo x rilasciare il TIVGD.
162200080930      *? Infine elimino il blocco elaborazione TIVGD x tipo file qui.  ?
162300080930     C                   clear                   trul47ds
162400080930     C                   eval      d47opz  = 'F'
162500080930     C                   eval      d47tip  = 'VC'
162600080930     C                   call      'TRUL47R'
162700080930     C                   parm                    trul47ds
162800080930      *
162900080930     C                   ENDSR     '*CANCL'
163000080930     C*------------------------------------------------------------------------*
163100971008**
163200970701Immettere codice cliente                                                      01
163300970701Codice cliente inesistente                                                    02
163400060811Codice cliente non presente in tabella 3K o PT                                03
163500970701Un altro utente sta chiudendo le spedizioni del cliente                       04
163600970701Immettere anno della spedizione                                               05
163700970701Immettere linea di partenza della spedizione                                  06
163800970701Linea di partenza inesistente                                                 07
163900970701Linea di partenza non gestita su questo as                                    08
164000970701Immettere numero della spedizione                                             09
164100970701Spedizione inesistente                                                        10
164200980206Bolla mamma: impossibile gestirla                                             11
164300980206Bolla figlia: impossibile gestirla                                            12
164400970701Spedizione già trasmessa alla filiale di arrivo                               13
164500970701Spedizione già trasmessa alla sede                                            14
164600970701Spedizione già trasmessa al cliente                                           15
164700980202Spedizione senza colli: impossibile eseguire chiusura                         16
164800980202Spedizione non appartenente al cliente selezionato                            17
164900980202Il codice selezionato deve appartenere ad un partner                          18
165000980202Codice evento inesistente                                                     19
165100160308Bolla in assegnato: occorre prima verificare ed annullare l'ORM               20
165200980202Codice evento obbligatorio                                                    21
165300980204Spedizione già consegnata                                                     22
165400980204Evento già inviato impossibile rinviarlo                                      23
165500990610Esistono C.A. aperte per la spedizione, codice evento non valido              24
165600131016 ATTENZIONE: più bolle con stesso riferimento!!                               25
165700131016 VERIFICARE se la Spedizione è quella corretta.                               26
165800150723Nessun dato da visualizzare per la selezione richiesta                        27
165900150728                                                                              28
166000150724Spedizione non gestibile:presenti spunte                                      29
166100150728Attenzione: caricate solo 9.999 bolle ma ne sono presenti ancora              30
166200150909Non trovato Evento per Chiusura per Merce mai Affidata. Contattare il CED (EM)31
