000100990610     H DECEDIT('0,') DATEDIT(*DMY.)
000200970410     H* FNLSI1R *----------------------------------------------------*
000300970410     H*                                                              *
000400970611     H*       - CHIUSURA BOLLA IN PARTENZA CON PRATICA IDD      -    *
000500050209     H*                                                              *
000600970410     H*--------------------------------------------------------------*
000700990810     FFNEVB04L  IF   E           K DISK
000800980204     F*--------
000900970612     FFNBLP01L  IF   E           K DISK
001000970410     F*--------
001100970612     FFNBLT01L  IF   E           K DISK
001101150724     FFNBrv07L  IF   E           K DISK
001200970410     F*--------
001300970410     FFNLBL01L  IF   E           K DISK
001400970410     F*--------
001500970410     FFNLBL02L  IF   E           K DISK
001600970611     F                                     RENAME(FNLBL000:FNLBL2)
001700970611     F*--------
001800130822     FFNBLp32L  IF   E           K DISK
001900130822     F                                     RENAME(FNBLP000:FNBLP32)
002000130822     F*--------
002100130822     FFNBLp33L  IF   E           K DISK
002200130822     F                                     RENAME(FNBLP000:FNBLP33)
002300130822     F*--------
002400970611     FCNACO00F  IF   E           K DISK
002500990610     F*--------
002600040715     F****FNDCT02L  IF   E           K DISK
002700970410     F*--------
002800970410     FTABEL00F  IF   E           K DISK    USROPN
002900970611     F*--------
003000970611     FEDTAB01L  IF   E           K DISK    USROPN
003100970410     F*--------
003200150724     FAZORG01L  IF   E           k disk
003300970414     F*--------
003400970414     FFNLSI1D   CF   E             WORKSTN
003401150723     f                                     sfile(lsi1S01 : S01nrr)
003500080918     F*--------
003600080918     FTNTBE01L  IF   E           K DISK
003700970410     D*--------------------------------------------------------------*
003800970410     D*  SCHIERE
003900970410     D*--------------------------------------------------------------*
004000970612     D*  Caricamento capoconto clienti
004100970410     D* Schiera codici tabella 3K: invio date consegna
004200030108     D C3K             S              7  0 DIM(9990)
004300030108     D CKS             S              7  0 DIM(9990)
004400030108     D CSD             S              2    DIM(9990)
004500970715     D* Tipi bolla: tabella 3A
004600970715     D C3A             S              2    DIM(500)
004700970715     D TPO             S              2    DIM(500)
004800970701     D* Schiera codici pistole fittizie: 7J
004900970701     D C7J             S              2  0 DIM(500)
005000970611     D* Schiera codici tabella PT
005100150415     D KEYPT           S             35    DIM(500)
005200970611     D KSC             S              7  0 DIM(500)
005300970616     D LNP             S              3  0 DIM(500)
005400970616     D NRS             S              2  0 DIM(500)
005500970616     D LAB             S              1    DIM(500)
005600150415     D* Schiera codici tabella CL
005700150415     D clKSC           S              7  0 DIM(999)
005800150415     D clLNP           S              3  0 DIM(999)
005900980202     D* Schiera codici tabella EM
006000980202     D ELB             S              6    DIM(500)
006100980202     D DLB             S             90    DIM(500)
006200980202     D EVG             S              6    DIM(500)
006300980202     D DVG             S             90    DIM(500)
006400970410     D* Caricamento £1
006500020506     D*UNI             S              3    DIM(29)
006600020506     D £1              S              3  0 DIM(30)
006700970414     D* ERRORI
006800150909     D ERR             S             78    DIM(31) CTDATA PERRCD(1)
006801150723
006802150723     d s01rcd          s                   like(c01rcd)
006803150723     d $Inzs01         s               n   inz(*on)
006804150723     d $Finerec        s               n   inz(*off)
006805150723     d wcev            s                   like(v2ccod)
006806150723     d wsts            s                   like(v2csts)
006807150727     d S01nrr          s              7  0 inz
006808150723     d Datadmy         s               d   datfmt(*dmy)
006809150724     d wtfp            s                   LIKE(BLPTFP)
006810150723     D WrkStringaSql   S           4500
006811150723     D                                     VARYING
006900970410     D*--------------------------------------------------------------*
007000970410     D* DS
007100970410     D*--------------------------------------------------------------*
007200030924     D Tibs36ds      E DS
007300970612     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
007400970612     D  TCU                  398    697
007500970612     D                                     DIM(50)
007600970612     D  KCU                  698    847P 0
007700970612     D                                     DIM(50)
007800970612     D                                     PACKEVEN
007900970612     D TCUDS           DS
008000970612     D  F3                     3      3
008100970612     D  F4                     4      4
008200970612     D  F56                    5      6
008300970612     D CNCR80        E DS
008400020506     d trul06ds      e ds
008500020506     d  lin                    1     90  0 dim(30)
008600970612     D* DS  appoggio x cmd
008700970616     D CMDAPP          DS            50
008800970616     D  NOMMBR                42     48
008900970617     D CMDLOG          DS            80
009000970617     D  MBRLOG                42     48
009100970617     D  MBRFIS                71     77
009101150724     d fnlv55ds      e ds
009200970414     D WLBDA8          DS
009300970414     D  G02DAT                 1      8  0
009400970414     D  G02INV                 9     16  0
009500970414     D  G02ERR                17     17
009600970414     D  G02TGI                18     22  0
009601150728     d WDAT8           DS
009602150728     d  datada                 1      8  0
009603150728     d  dataa                  9     16  0
009604150728     d  ggl                   17     21  0
009700080421      *----------------------------------------------------------------
009800080421      * Controllo e semaforo x scrittura del VAC: TRUL47R
009900080421      *----------------------------------------------------------------
010000080421     D trul47ds      e ds
010100080421      *
010200970410     D KPJBA         E DS
010201150723     d fnblpds       e ds                  extname(fnblp00f) inz
010300970612     D DSPT          E DS                  EXTNAME(EDIDSPT)
010400970616     D DSPU          E DS                  EXTNAME(EDIDSPU)
010500980202     D DSEM          E DS                  EXTNAME(EDIDSEM)
010600150415     D DSCL          E DS                  EXTNAME(EDIDSCL)
010700970616     D* DATI X RICHIAMO PGM INTERR. BOLLE PARTENZE (FNLS05R)
010800970616     D PARA04          DS                  INZ
010900970616     D  P04AAS                 1      4  0
011000970616     D  P04LNP                 5      7  0
011100970616     D  P04NRS                 8      9  0
011200970616     D  P04NSP                10     16  0
011300980204     D  P04F03                17     17
011400970616     D  P04FLG                31     31
011500970616     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
011600970616     D  P04GIA                48     48
011700970616      * PA3CLI indica se il pgm e' richiamato da un cliente (='S')
011800970616     D  P04CLI               143    143
011900970616     D PARA05          DS                  INZ
012000970616     D  P05AAS                 1      4  0
012100970616     D  P05LNP                 5      7  0
012200970616     D  P05NRS                 8      9  0
012300970616     D  P05NSP                10     16  0
012400970616     D  P05F03                17     17
012500970616     D  P05FLG                18     18
012600970616     D  P05SIM                44     46  0
012700970616     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
012800970616     D  P05GIA                48     48
012900080421     D***** TIBS55        E DS                  EXTNAME(TIBS55DS)
013000971126     D FNLSI1        E DS                  EXTNAME(FNLSI1DS)
013100970410     D DS3K          E DS
013200970715     D DS3A          E DS
013300970701     D DS7J          E DS
013400970410     D DS7O          E DS
013401150728     D DS3i          E DS
013500060811     D OG143         E DS
013600040715
013700040715     d Fidn12ds      e ds
013800040715     d  skKey                 26    305    dim(20)
013900040715     d  skAnn                306    325    dim(20)
014000040715     d  skDca                326    485    dim(20)
014100040715     d  skFca                486    545  0 dim(20)
014200040715     d  skDch                546    705  0 dim(20)
014300040715     d  skCch                706    745    dim(20)
014400040715
014500970410     D                SDS
014600970410     D  NOMPGM                 1     10
014700970410     I*--------------------------------------------------------------*
014800970410     I*  Definizione campi Azorg
014900970410     I*--------------------------------------------------------------*
015000150724     I***AZORG01L  AA
015100150724     I***                                  1    3  ORGDIT
015200150724     I***                             P    4    5 0ORGFIL
015300150724     I***                               3776 3800  ORGDE3
015301150728      /copy gaitrasrc/srcprotopr,xsrlav8
015400970410     C*--------------------------------------------------------------*
015500970410     C*  C I C L O     P R I N C I P A L E
015600970410     C*--------------------------------------------------------------*
015601150723      /free
015602150723         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
015603150723      /end-free
015700970410     C*  Inizializzo varibili videata
015800970410     C                   EXSR      INZ01
015900080421      *
016000970410     C*  Gestione della prima videata
016100970410     C     WFINE         DOWEQ     'N'
016200970611     C     WTPVID        CASEQ     '1'           GESD01
016300970611     C     WTPVID        CASEQ     '2'           GESD02
016301150723     C     WTPVID        CASEQ     'S'           GESsfl
016400970410     C                   END
016500970611     C                   END
016600970410     C*  Fine pgm
016700970410     C                   SETON                                        LR
016800970611     C*--------------------------------------------------------------*
016900970611     C* INZ01: Impostazione della prima videata
017000970611     C*--------------------------------------------------------------*
017100970611     C     INZ01         BEGSR
017200970611     C*
017300970611     C                   SETOFF                                       2840
017400970616     C                   MOVEL     '1'           WTPVID
017500970617     C                   MOVEL     'N'           WRIC81            1
017600970616     C                   MOVEL     'N'           WFINE
017700970611     C                   MOVEL     NOMPGM        V1CPGM
017800970611     C                   Z-ADD     0             V1CKSC
017900970611     C*
018000970611     C                   ENDSR
018100970410     C*--------------------------------------------------------------*
018200970611     C* INZ02: Impostazione della prima videata
018300970410     C*--------------------------------------------------------------*
018400970611     C     INZ02         BEGSR
018500970410     C*
018600970410     C                   SETOFF                                       284041
018700970611     C                   SETOFF                                       424344
018800970616     C                   MOVEL     '2'           WTPVID
018900970611     C                   MOVEL     NOMPGM        V2CPGM
019000970611     C                   MOVEL     V1CKSC        V2CKSC
019100970611     C                   MOVEL     ACORAG        V2DKSC
019200980204     C                   MOVEL     *BLANKS       V2DCOD
019300980204     C                   MOVEL     *BLANKS       V2CCOD
019400980202     C                   MOVEL     *BLANKS       V2CSTS
019500980202     C                   MOVEL     *BLANKS       V2CFTX
019600970611     C                   MOVE      WHHDAT        V2CAAS
019700130304      **  gestione cliente con Partner gestito da altra filiale
019800130304      **  vedi GWEISS con clienti sulla 277.
019900130304     c                   if        LNP_ALTRA_FIL = 0
020000971007     C                   Z-ADD     LNP(XY)       V2CLNP
020100130821      ******
020200130821     c**** Chiodo x Ducros da Eliminare fra 333 e 330
020300130821     c                   if        v2clnp =333
020400130821     c                   eval      v2clnp =330
020500130821     c                   end
020600130821      ******
020700130304     c                   else
020800130304     C                   eval        V2CLNP = LNP_ALTRA_FIL
020900130304     c                   end
021000971007     C                   Z-ADD     NRS(XY)       V2CNRS
021100970611     C                   Z-ADD     0             V2CNSP
021200970410     C*
021300970410     C                   ENDSR
021400970410     C*--------------------------------------------------------------*
021500970410     C* GESD01: Gestione della prima videata
021600970410     C*--------------------------------------------------------------*
021700970410     C     GESD01        BEGSR
021800970410     C*
021900970410     C* Eseguo exfmt della prima videata
022000970410     C                   EXFMT     LSI1D01
022100970611     C                   SETOFF                                       2840
022200970410     C                   SETOFF                                       4243
022300970410     C* Fine
022400970410     C     *INKC         IFEQ      '1'
022500970414     C                   MOVEL     'S'           WFINE             1
022600970410     C                   GOTO      FINVD1
022700970410     C                   END
022800970410     C* Controlli
022900970410     C                   EXSR      CTR01
023000970410     C   28              GOTO      FINVD1
023100970611     C*
023200970611     C* Se no errori passo a videata successiva
023201150723     c* chiusura bolle per merce mai affidata --> la videata successiva è il sfl
023202150723     c* delle bolle
023203150723     c                   if        v1cfbo='S'
023204150723     C                   MOVEL     'S'           WTPVID
023205150723     c                   eval      $inzS01=*on
023206150723     c                   else
023300150724     C                   EXSR      INZ02
023301150723     C                   MOVEL     '2'           WTPVID
023302150723     c                   endif
023400970410     C*
023500970410     C     FINVD1        ENDSR
023600970611     C*--------------------------------------------------------------*
023700970611     C* GESD02: Gestione della seconda videata
023800970611     C*--------------------------------------------------------------*
023900970611     C     GESD02        BEGSR
024000970611     C*
024100970611     C* Eseguo exfmt della prima videata
024200970611     C                   EXFMT     LSI1D02
024300970611     C                   SETOFF                                       284041
024400970611     C                   SETOFF                                       424344
024500970611     C* Fine
024600970611     C     *INKC         IFEQ      '1'
024700970611     C                   MOVEL     'S'           WFINE             1
024800970617     C* Controllo se devo richiamare TRTC81
024900970617     C     WRIC81        IFEQ      'S'
025000970617     C                   EXSR      CALL81
025100970617     C                   END
025200970611     C                   GOTO      FINVD2
025300970611     C                   END
025400970611     C* Prima videata
025500970611     C     *INKL         IFEQ      '1'
025600970611     C                   MOVEL     '1'           WTPVID            1
025700970617     C* Controllo se devo richiamare TRTC81
025800970617     C     WRIC81        IFEQ      'S'
025900970617     C                   EXSR      CALL81
026000970617     C                   END
026100970611     C                   GOTO      FINVD2
026200970611     C                   END
026300080421      *
026400970612     C* Interrogazione bolle
026500970612     C     *INKG         IFEQ      '1'
026600970616     C     V2CNSP        IFGT      0
026700970616     C                   CLEAR                   PARA05
026800970616     C                   Z-ADD     V2CAAS        P05AAS
026900970616     C                   Z-ADD     V2CLNP        P05LNP
027000970616     C                   Z-ADD     V2CNRS        P05NRS
027100970616     C                   Z-ADD     V2CNSP        P05NSP
027200970616     C                   MOVEL     '1'           P05GIA
027300970616     C                   MOVEL     PARA05        KPJBU
027400970616     C                   CALL      'FNLS05R'
027500970616     C                   PARM                    KPJBA
027600970616     C                   MOVEL     KPJBU         PARA05
027700980204     C     P05F03        IFEQ      *BLANKS
027800980204     C                   Z-ADD     P05AAS        V2CAAS
027900980204     C                   Z-ADD     P05LNP        V2CLNP
028000980204     C                   Z-ADD     P05NRS        V2CNRS
028100980204     C                   Z-ADD     P05NSP        V2CNSP
028200980204     C                   ENDIF
028300970616     C*
028400970616     C                   ELSE
028500970616     C*
028600970616     C                   CLEAR                   PARA04
028700970616     C                   MOVEL     '1'           P04FLG
028800970616     C                   MOVEL     PARA04        KPJBU
028900970616     C                   CALL      'FNLS04R'
029000970616     C                   PARM                    KPJBA
029100980204     C                   MOVEL     KPJBU         PARA04
029200980204     C     P04F03        IFEQ      *BLANKS
029300980204     C                   Z-ADD     P04AAS        V2CAAS
029400980204     C                   Z-ADD     P04LNP        V2CLNP
029500980204     C                   Z-ADD     P04NRS        V2CNRS
029600980204     C                   Z-ADD     P04NSP        V2CNSP
029700970616     C                   ENDIF
029800980204     C                   ENDIF
029900970612     C                   GOTO      FINVD2
030000970612     C                   END
030100970611     C* Controlli
030200970611     C                   EXSR      CTR02
030300970611     C   28              GOTO      FINVD2
030400970611     C*
030500970611     C* Se no errori vado a pgm successivo
030600970611     C                   EXSR      CALPGM
030700970616     C* Azzero prima videata se ho eseguito conferma
030800970616     C     DI1CMD        IFEQ      '06'
030900970617     C                   EXSR      INZ02
031000970616     C                   END
031100970611     C*
031200970611     C     FINVD2        ENDSR
031300970611     C*--------------------------------------------------------------*
031400970611     C* CTR01: Controlli prima videata
031500970611     C*--------------------------------------------------------------*
031600970611     C     CTR01         BEGSR
031700970611     C*
031800970611     C* Controllo se immesso codice cliente
031900970611     C     V1CKSC        IFEQ      0
032000970611     C                   SETON                                        2840
032100970611     C                   MOVEL     ERR(1)        $MSG
032200970611     C                   END
032300990610     C   28              GOTO      FINCT1
032400150415      *
032500970611     C* Controllo se esiste cliente
032600970612     C                   Z-ADD     1             KKUT
032700970611     C                   Z-ADD     CLIITA        KKCC
032800970611     C                   Z-ADD     V1CKSC        KKSC
032900970611     C     KACO          CHAIN     CNACO00F                           31
033000970611     C     *IN31         IFEQ      '1'
033001150723     c                   clear                   vhdksc
033100970611     C                   SETON                                        2840
033200970611     C                   MOVEL     ERR(2)        $MSG
033300970611     C                   END
033400990610     C   28              GOTO      FINCT1
033401150723     c                   movel     acorag        vhdksc
033500150415      ****
033600970617     C* Controllo Codice cliente unificante tabella 3K
033700030116     C                   Z-ADD     1             XX                5 0
033800970617     C     V1CKSC        LOOKUP    C3K(XX)                                32
033900060811      *
034000060811      **?se non ha agganciato nulla
034100060811     c                   if        *in32 = *off
034200150415      *
034300060811     c     v1cksc        div       10000         trelnp            3 0
034400060811     C                   Z-ADD     treLNP        ORGFIL
034500060811     C     ORGFIL        CHAIN     AZORG01L                           31
034600060811      **? e se  si tratta di DPD (eccezione .......)
034700060811     c                   if        %Found(AZORG01L)
034800060811     c                   movel     orgde3        og143
034900060811     c                   if        §OGNTW ='DPD'
035000060811     C                   Z-ADD     1             XY                5 0
035100150415      ** va direttamente sulla PT poichè DPD non sta sulla 3K
035200060811     C     V1CKSC        LOOKUP    KSC(XY)                                32
035300060811     c                   end
035400060811     c                   end
035500060811      **
035600060811     c                   else
035700060811      **
035800150415      **  con l'unificante della 3K
035900970617     C* Controllo se cliente in tabella PT
036000060811     C                   Z-ADD     1             XY                5 0
036100060811     C     CKS(XX)       LOOKUP    KSC(XY)                                32
036200150415      **
036300060811     c                   end
036400060811      **
036500150415      **  Se non l'avesse trovato con l'UNIFICANTE sulla PT
036600970611     C     *IN32         IFEQ      '0'
036700150415      **
036800970611     C                   SETON                                        2840
036900970611     C                   MOVEL     ERR(3)        $MSG
037000150415      **
037100970617     C                   ELSE
037200020912      *
037300020912      *  la Linea di Partenza del Partner deve essere gestita
037400020912      *   nella £1
037500020912     C     LNP(XY)       lookup    £1                                     32
037600080918      ** ----
037700130304      **     Potrebbe essere un cliente appartenente alla tabella CL
037800130304      **     gestito da un'altra filiale. a questo punto occorre controllare,
037900130304      **     non solo la LINEA del PARTNER agganciata sull'unificante della 3K
038000130304      **     ma se non fosse gestito il Partner sulla filiale si deve vedere
038100130304      **     la Linea del Cliente se è gestita sulla filiale.
038200130304      **   (Esempio su clienti di GWeiss della 360 dati in gestione alla 277.
038300130304      **    il cliente è il 2772159 unificato sulla 3K al 3600001.
038400130304      **   Non è gestita la 360 nella 277 allora il cliente 2772159 è della 277
038500130304      **    quindi deve passare il controllo...poichè il cliente è in gestione)
038600130304     c                   clear                   LNP_ALTRA_FIL     3 0
038700150415      **
038800130304     c                   if        NOT *in32
038900150415      **
039000150415      ** Dovendo gestire casi in cui il cliente è stato spostato ANCORA di
039100150415      **  Filiale mantenendo il codice cliente, occorre effettuare il
039200150415      ** ragionamento sulla LNP presente sulla tab.CL o sulla PT per
039300150415      ** verificare l'appartenenza alla £1 e NON con i primi 3 caratteri del
039400150415      ** codice cliente.
039500150415      ** E'sempre il caso della GWEISS che adesso da 277 viene spostato alla
039600150415      **  249 mantenendo il codice cliente come 277xxxx e facendo capo alla 360
039700150415      **
039800150415     c                   clear                   treLNP            3 0
039900150415     c**** v1cksc        div       10000         treLNP            3 0
040000150415      ** ---
040100150415      **  cerca sulla CL  x impostare la LNP
040200150415     C                   Z-ADD     1             clpt              5 0
040300150415     C     V1CKSC        LOOKUP    clKSC(clpt)                            32
040400150415     c                   if        *in32
040500150415     c                   z-add     clLNP(clpt)   treLNP
040600150415     c                   else
040700150415      **  se non la trova cerca sulla PT  x impostare la LNP
040800150415     C                   Z-ADD     1             clpt              5 0
040900150415     C     V1CKSC        LOOKUP    KSC(clpt)                              32
041000150415     c   32              z-add     LNP(clpt)     treLNP
041100150415     c                   end
041200150415      ** ---
041300150415      **
041400130304     C     treLNP        lookup    £1                                     32
041500130304     c   32              movel     treLNP        LNP_ALTRA_FIL
041600150415      **
041700130304     c                   end
041800130304      ** ----
041900080918      ** Occorre anche controllare se DPD di backup quindi linee collegate
042000080918      **  per potere gestire linee da filiali diverse
042100080918      **   ossia tab."3NN"
042200080918      **
042300080918     C     *IN32         ifeq      *off
042400080918     c                   movel(p)  '3NN'         TbeCOD
042500080918     c                   movel(p)  KSC(xy)       TbeKE1
042600080918     c     kTBE1         setll     tntbe01l
042700080918     c     kTBE1         reade     tntbe01l
042800080918     C                   dow       not %EoF(tntbe01l)
042900080918     c                   movel(p)  tbeKE2        LNPAlternativa    3 0
043000080918     C     LNPAlternativalookup    £1                                     32
043100080918     c                   if        *in32
043200080918     c                   z-add     1             z                 3 0
043300080918     c     *zero         lookup    £1(z)                                  21
043400080918      * aggiunge la linea x il controllo successivo
043500080918     c                   if        *in21
043600080918     C                   z-add     lnp(xy)       £1(z)
043700080918     C                   End
043800080918     C                   Leave
043900080918     C                   End
044000080918     c     kTBE1         reade     tntbe01l
044100080918     C                   End
044200080918     C                   End
044300080918      ** ----
044400020912     C     *IN32         ifeq      *off
044500020912     C                   SETON                                        2840
044600020912     C                   MOVEL     err(8)        $MSG
044700020912     C                   Else
044800020912     C*
044900020912     C                   EndIF
045000020912     C*
045100970617     C                   END
045200020912     C*
045300990610     C   28              GOTO      FINCT1
045301150909     c* Se richiesta visualizzazione bolle da chiudere per merce mai
045302150909     c* affidata ERRORE se non trovato l'evento su tabella EM
045303150909     c                   if        v1cfbo='S' and wsts=*blanks
045304150909     C                   SETON                                        28
045305150909     C                   MOVEL     err(31)       $MSG
045306150909     c                   endif
045400080421     C*
045500970611     C     FINCT1        ENDSR
045501150723     C*--------------------------------------------------------------*
045502150723     C* gessfl:  videata sfl per codice cliente
045503150723     C*--------------------------------------------------------------*
045504150723     C     gessfl        BEGSR
045505150723      /free
045506150723         if $inzs01=*on;
045507150723            exsr inzs01;
045508150723            c01csr=1;
045509150723            c01rcd=1;
045510150723            $inzs01=*off;
045511150723         // Se non trovati record errore in prima videata
045512150723            if s01nrr=0   ;
045513150723               $msg=err(27)  ;
045514150723               *in28=*on  ;
045515150723               *in40=*on  ;
045516150723               wtpvid='1'  ;
045517150723               leavesr   ;
045518150723            endif  ;
045522150723            if s01nrr=1;
045523150724              chain (vshaas:vshlnp:vshnrs:vshnsp) fnblp01l ;
045524150723              exsr calpgm;
045525150723              leavesr;
045526150723            endif;
045527150723         endif;
045528150723       // c01csr   contiene il numero di riga del subfile su cui era posizionato
045529150723       // impostando c01rcd   visualizzo la pagina che vedeva l'utente quando ha
045530150723       // l'ultimo tasto
0455311507232         if c01csr   > 0;
045532150723           c01rcd   = c01csr;
045533150723          else;
045534150723           c01rcd   = s01rcd   ;
0455351507232         endif;
045536150723       // se non so quale pagina visualizzare forzo pagina 1
0455371507232         if c01rcd   < 1;
045538150723           eval c01rcd   = 1;
0455391507232         endif;
045540150723
045541150723       // valorizzo campi del sfl control
045542150723          vc1ksc=%int(v1cksc);
045543150724          vc1kscd=vhdksc;
045544150723
045545150723
045546150723         // Emissione videata
045547150723         write  lsi1z01;
045548150723         exfmt  lsi1C01;
045549150723
045550150723         s01nrr=1  ;
045551150723
045552150723 1       SELECT;
045553150723
045554150723         // - F3=Fine
045555150723 1         WHEN  *inkc;
045556150723               wFine = 'S';
045557150723
045558150723         // - F12=Ritorno
045559150723 1         WHEN  *inkl;
045560150914               *in28=*off;
045561150723               wtpvid='1'  ;
045562150723
045563150723
045564150723         // -
045565150723 1       //WHEN  *inkb ;
045566150723         //  exsr seltot;
045567150723
045568150723
045569150723 x1      // Invio
045570150723           OTHER;
045571150723
045572150723               exsr ContrS01 ;
045573150723
045574150723 1       ENDSL;
045575150723      /end-free
045576150723     c                   endsr
045577150723      /free
045578150723       //--------------------------------------------------------------
045579150723       BEGSR InzS01;
045580150723
045581150723       // Pulizia subfile
045582150723         *in30  = *on;
045583150723         *in31  = *on;
045584150723         write  lsi1c01;
045585150723         *in31  = *off;
045586150723         *in30  = *off;
045587150723         *in32  = *off;
045588150723
045589150723         exec sql close BLPcsr;
045590150723       // Richiamo routine di preparazione stringa SQL
045591150723             exsr sr_prepsql;
045592150723
045593150723         S01nrr=0 ;
045594150723         clear $msg;
045595150723         *in28  = *off;
045596150723         *in90  = *off;
045597150728         *in19=*off;
045598150723       // Elaboro i records estratti
045599150723           $finerec=*off;
045600150723           exec sql prepare STRINGASQL from :WrkStringaSql;
045601150723           exec sql declare BLPCSR cursor for StringaSql;
045602150723           exec sql open BLPcsr;
045603150723           dow $finerec=*off;
045604150723              exec sql Fetch BLPcsr into :fnblpds;
045605150728              *in28  = *off;
045606150728              if sqlcod=100 or sqlcod<0;
045607150723                 $finerec = *on;
045608150723                 leave;
045609150723              endif;
045610150728              if s01nrr=9999;
045611150728                 $finerec = *on;
045612150728                 $msg=err(30);
045613150728                 *in28=*on;
045614150728                 leave;
045615150728              endif;
045616150723              if %lookup(blplnp:£1)>0;
045617150724                 kaas=blpaas;
045618150724                 klnp=blplnp;
045619150724                 knrs=blpnrs;
045620150724                 knsp=blpnsp;
045621150728       //per poter chiudere la spedizione come merce mai affidata
045622150728       //devono essere trascorsi i giorni max di sospensione previsti
045623150728       //da tabella
045624150728                 clear wdat8;
045625150728                 datada=blpdim;
045626150728                 dataa=(blpaas*10000)+blpmgs;
045627150728                 xsrlav8(wdat8);
045628150728                 if (ggl-1) >  §3iggc;
045629150728                    exsr ctr_sfl;
045631150728                    if not *in28;
045632150728           // Carico record nel sfl
045633150728                       exsr carsf01;
045634150728                    endif;
045635150728                 endif;
045636150723              endif;
045637150723           enddo;
045638150723
045639150723          s01rcd    = s01nrr  ;
045640150723
045641150723         ENDSR  ;
045642150723       //--------------------------------------------------------------
045643150723       //?Preparazione stringa SQL
045644150723       //--------------------------------------------------------------
045645150723       BEGSR sr_prepsql;
045646150723       WrkStringaSql='select * from fnblp00f' +
045647150724       ' where (blpksc=' + %editc(v1cksc:'X') + ' or '      +
045648150724       ' blpccm=' + %editc(v1cksc:'X') +
045649150724       ') and blptfp=' + %editc(wtfp:'X') +
045650150723       ' and blpnfv=0' +
045651150723       ' order by  blptfp, blpnfv, blpdim';
045652150723       endsr;
045653150723       //--------------------------------------------------------------
045654150723       //?eseguo caricamento SFL
045655150723       //--------------------------------------------------------------
045656150723          BEGSR   carSF01    ;
045657150723          clear vs1opz  ;
045658150723
045659150723          exsr CarRECORD  ;
045660150723          vshaas=blpaas;
045661150723          vshlnp=blplnp;
045662150723          vshnrs=blpNRs;
045663150723          vshnsp=blpNsp;
045664150723          s01nrr=s01nrr+1   ;
045665150723          write  lsi1S01;
045666150723
045667150723          ENDSR         ;
045668150723       //--------------------------------------------------------------
045669150723       //?Caricamento record SFL
045670150723       //--------------------------------------------------------------
045671150723       BEGSR CarRecord;
045672150723       // data Immissione bolla
045673150723             clear vs1dim;
045674150723             datadmy=%date(blpdim:*iso);
045675150723             vs1dim=%dec(datadmy);
045676150723       // Data Spedizione
045677150723             clear vs1dsp;
045678150723             eval datadmy=%date((blpaas*10000+blpmgs):*iso);
045679150723             vs1dsp=%dec(datadmy);
045680150723       // linea di arrivo
045681150723             vs1lna=blplna;
045682150723             chain blplna azorg01l ;
045683150723             if orgde5<>*blanks;
045684150723                 vs1lnad=%subst(orgde5:1:10);
045685150723             else;
045686150723                 vs1lnad=%subst(orgdes:1:10);
045687150723             endif;
045688150723       // Destinatario
045689150723             vs1rsd=blprsd;
045690150723       ENDSR         ;
045691150723       //--------------------------------------------------------------
045692150723       //?controlli SFL
045693150723       //--------------------------------------------------------------
045694150723       BEGSR ContrS01;
045695150723
045696150723       s01nrr=1  ;
045697150723       readc lsi1s01    ;
045698150723
045699150723 0     dow not %eof                      ;
045700150723
045701150723       // Verifica l'opzione
045702150723          if vs1opz<>*blanks    ;
045703150723            *in19=*off;
045704150723          select;
045705150723          when vs1opz='1'                     ;
045706150724            chain (vshaas:vshlnp:vshnrs:vshnsp) fnblp01l ;
045707150914                 kaas=blpaas;
045708150914                 klnp=blplnp;
045709150914                 knrs=blpnrs;
045710150914                 knsp=blpnsp;
045711150728            exsr ctr_sfl;
045712150724            if *in28;
045713150724               eval *in19=*on;
045714150724            else;
045715150724               exsr calpgm;
045716150724            endif;
045717150724            chain (vshaas:vshlnp:vshnrs:vshnsp) fnblp01l ;
045718150723            if blpdcm>0;
045719150723               eval *in19=*on;
045720150723            endif;
045721150723          when vs1opz='5';
045722150723               exsr int_bolla;
045723150723          endsl;
045724150723               clear vs1opz;
045725150723               update  lsi1s01  ;
045726150723          endif;
045727150723
045728150723       readc    lsi1s01    ;
045729150723 0     enddo         ;
045730150723
045731150723       ENDSR  ;
045732150723       //--------------------------------------------------------------
045733150723       //?Aggiornamento sfl
045734150723       //--------------------------------------------------------------
045735150723       BEGSR AGGIOSFL  ;
045736150723
045737150723       //Sflnxtchg=*on  ;
045738150723
045739150723       update  lsi1s01  ;
045740150723
045741150723       ENDSR   ;
045742150723      /end-free
045743150723     C*--------------------------------------------------------------*
045744150723     C* interroga bolla in partenza
045745150723     C*--------------------------------------------------------------*
045746150723     C     int_bolla     BEGSR
045747150723     C                   CLEAR                   PARA05
045748150723     C                   Z-ADD     VshAAS        P05AAS
045749150723     C                   Z-ADD     VshLNP        P05LNP
045750150723     C                   Z-ADD     VshNRS        P05NRS
045751150723     C                   Z-ADD     VshNSP        P05NSP
045752150723     C                   MOVEL     '1'           P05GIA
045753150723     C                   MOVEL     PARA05        KPJBU
045754150723     C                   CALL      'FNLS05R'
045755150723     C                   PARM                    KPJBA
045756150723     c                   endsr
045757970410     C*--------------------------------------------------------------*
045758130822     C* Decodifica i 2 riferimenti per impostare la spedizione
045800970410     C*--------------------------------------------------------------*
045900130822     C     decod_da_Rif  BEGSR
046000970410     C*
046100131016     c                   clear                   piu_bolle_Xrif    1
046200131016     c                   clear                   V2NOTE1
046300131016     c                   clear                   V2NOTE2
046400131016     C*
046500130822     c                   clear                   se_trovato        1
046600130822     c                   eval      se_trovato ='N'
046700130822     C*
046800130822     c                   if        v2rif32 <> *blank
046900131016     c     v2rif32       setll     fnblp32l
047000131016     c     v2rif32       reade     fnblp32l
047100131016     c                   dow       not %EoF(fnblp32l)
047200130822     c                   eval      se_trovato ='S'
047300131016     c     v2rif32       reade     fnblp32l
047400131016     c                   if        not %EoF(fnblp32l)
047500131016     c                   eval       piu_bolle_Xrif ='S'
047600131016     c                   end
047700130822     c                   end
047800130822     c                   end
047900130822      *
048000130822     c                   if        v2rif33 > 0 and
048100130822     c                             se_trovato ='N'
048200131016     c     v2rif33       setll     fnblp33l
048300131016     c     v2rif33       reade     fnblp33l
048400131016     c                   dow       not %Eof(fnblp33l)
048500130822     c                   eval      se_trovato ='S'
048600131016     c     v2rif33       reade     fnblp33l
048700131016     c                   if        not %EoF(fnblp33l)
048800131016     c                   eval       piu_bolle_Xrif ='S'
048900131016     c                   end
049000130822     c                   end
049100130822     c                   end
049200130822     C*
049300130822     C* Se lo ha trovato imposta imposta i campi della spedizione
049400131016     C*   ATTENZIONE potrebbero essercene più di una con lo stesso
049500131016     C* riferimento quindi segnala che ha preso l'ultima
049600130822     c                   IF        se_trovato ='S'
049700131016     c                               and v2cnsp = 0
049800130822     c                   eval      v2caas = blpaas
049900130822     c                   eval      v2clnp = blplnp
050000130822     c                   eval      v2cnrs = blpnrs
050100130822     c                   eval      v2cnsp = blpnsp
050200130822     c                   end
050300131016     C*   AVVISA SE HA trovato più bolle con lo stesso riferimento
050400131016     c                   IF         piu_bolle_Xrif ='S'
050500131016     c                   EVAL        V2NOTE1 = err(25)
050600131016     c                   EVAL        V2NOTE2 = err(26)
050700131016     c                   end
050800131016     C*
050900130822     C                   ENDSR
051000130822     C*--------------------------------------------------------------*
051100130822     C* CTR02: Controlli prima videata
051200130822     C*--------------------------------------------------------------*
051300130822     C     CTR02         BEGSR
051400130822     C*
051500130822     C*  pre- controllo bolla tramite rif.alfa o numerico
051600130822     c                   exsr      decod_da_Rif
051700130822     C*
051800970410     C* Controllo se immesso anno
051900970611     C     V2CAAS        IFEQ      0
052000970410     C                   SETON                                        2840
052100970611     C                   MOVEL     ERR(5)        $MSG
052200970410     C                   END
052300990610     C   28              GOTO      FINCT2
052400970410     C* Se anno digitato di 2 --> 4 cifre
052500970612     C                   MOVEL     V2CAAS        WSECOL            2 0
052600970410     C     WSECOL        IFEQ      0
052700970611     C     V2CAAS        IFGT      60
052800970611     C                   ADD       1900          V2CAAS
052900970410     C                   ELSE
053000970611     C                   ADD       2000          V2CAAS
053100970410     C                   END
053200970410     C                   END
053300970410     C* Controllo se immessa linea di partenza
053400970611     C     V2CLNP        IFEQ      0
053500970414     C                   SETON                                        2841
053600970611     C                   MOVEL     ERR(6)        $MSG
053700970410     C                   END
053800990610     C   28              GOTO      FINCT2
053900970410     C* Controllo esistenza lnp
054000970611     C                   Z-ADD     V2CLNP        ORGFIL
054100970410     C     ORGFIL        CHAIN     AZORG01L                           31
054200970611     C     *IN31         IFEQ      '1'
054300970410     C                   SETON                                        2841
054400970611     C                   MOVEL     ERR(7)        $MSG
054500970414     C                   END
054600990610     C   28              GOTO      FINCT2
054700970414     C* Controllo se lnp in £1
054800970611     C     V2CLNP        LOOKUP    £1                                     32
054900970414     C     *IN32         IFEQ      '0'
055000970414     C                   SETON                                        2841
055100970611     C                   MOVEL     ERR(8)        $MSG
055200970414     C                   END
055300990610     C   28              GOTO      FINCT2
055400970414     C* Controllo se immesso numero di spedizione
055500970611     C     V2CNSP        IFEQ      0
055600970414     C                   SETON                                        2843
055700970611     C                   MOVEL     ERR(9)        $MSG
055800970414     C                   END
055900990610     C   28              GOTO      FINCT2
056000970414     C* Controllo se esiste spedizione
056100970611     C                   Z-ADD     V2CAAS        KAAS
056200970611     C                   Z-ADD     V2CLNP        KLNP
056300970611     C                   Z-ADD     V2CNRS        KNRS
056400970611     C                   Z-ADD     V2CNSP        KNSP
056500970414     C     KBLP          CHAIN(N)  FNBLP01L                           31
056600970414     C     *IN31         IFEQ      '1'
056700970414     C                   SETON                                        2843
056800970611     C                   MOVEL     ERR(10)       $MSG
056900970616     C                   ELSE
057000970616     C* Controllo che la spedizione sia del cliente selezionato
057100970616     C     BLPCCM        IFNE      V2CKSC
057200970616     C     BLPKSC        ANDNE     V2CKSC
057300970616     C                   SETON                                        2843
057400980202     C                   MOVEL     ERR(17)       $MSG
057500970414     C                   END
057600970616     C                   END
057700990610     C   28              GOTO      FINCT2
057800970414     C* Controllo se bolla mamma
057900150728     C*                  Z-ADD     V2CAAS        KAAP
058000150728     C*                  Z-ADD     V2CLNP        KLNP
058100150728     C*                  Z-ADD     V2CNRS        KNRP
058200150728     C*                  Z-ADD     V2CNSP        KNSP
058300150728     C*    KBLP          CHAIN     FNLBL02L                           31
058400970414     C* Controllo se bolla figlia
058500150728     C*                  Z-ADD     V2CAAS        KAAP
058600150728     C*                  Z-ADD     V2CLNP        KLNP
058700150728     C*                  Z-ADD     V2CNRS        KNRP
058800150728     C*                  Z-ADD     V2CNSP        KNSP
058900150728     C*    KBLP          CHAIN     FNLBL01L                           31
059000150728     C*    *IN31         IFEQ      '0'
059100150728     C*                  SETON                                        284344
059200150728     C*                  MOVEL     ERR(12)       $MSG
059300150728     C*                  END
059400150728     C*  28              GOTO      FINCT2
059500980204     C* Controllo che non sia stata data per consegnata
059600150728     C*    BLPDCM        IFNE      0
059700150728     C*    BLPCCA        ANDNE     '6'
059800150728     C*    BLPDCM        ORNE      0
059900150728     C*    BLPCCA        ANDNE     '2'
060000150728     C*                  SETON                                        2843
060100150728     C*                  MOVEL     ERR(22)       $MSG
060200150728     C*                  END
060300150728     C*  28              GOTO      FINCT2
060400980204     C* Controllo che abbia il dettaglio segnacolli
060500150728     C*                  Z-ADD     V2CAAS        KAAS
060600150728     C*+                 Z-ADD     V2CLNP        KLNP
060700150728     C**                 Z-ADD     V2CNRS        KNRS
060800150728     C*                  Z-ADD     V2CNSP        KNSP
060900150728     C*    KBLT          CHAIN     FNBLT01L                           31
061000150728     C*    *IN31         IFEQ      '1'
061100150728     C*                  SETON                                        2843
061200150728     C*                  MOVEL     ERR(16)       $MSG
061300150728     C*                  END
061301150728     c                   exsr      ctr_uniS
061400150728     C   28              GOTO      FINCT2
061500980202     C* Controllo se richiesto '?'
061600980202     C     '?'           SCAN      V2CSTS                                 32
061700980202     C     *IN32         IFEQ      '1'
061800980202     C                   MOVE      LAB(XY)       DI1LAB
061900980202     C                   MOVEL     FNLSI1        KPJBU
062000980203     C                   CALL      'FNLSI1R5'
062100980202     C                   PARM                    KPJBA
062200980204     C                   MOVEL     KPJBU         FNLSI1
062300980204     C                   MOVEL     DI1CEV        V2CCOD
062400980202     C                   MOVEL     DI1STS        V2CSTS
062500980202     C                   END
062600980202     C* evento obbligatoria
062700980202     C     V2CCOD        IFEQ      *BLANKS
062800980202     C     V2CSTS        ANDEQ     *BLANKS
062900970721     C                   SETON                                        2844
063000980202     C                   MOVEL     ERR(21)       $MSG
063100970721     C                   END
063200990610     C   28              GOTO      FINCT2
063300980202     C* evento esistente
063400980202     C                   MOVEL     V2CSTS        WEVE              6
063500980202     C                   MOVE      V2CCOD        WEVE
063600030116     C                   Z-ADD     1             YY                5 0
063700980202     C     LAB(XY)       IFEQ      'S'
063800980202     C     WEVE          LOOKUP    ELB(YY)                                32
063900980202     C                   MOVEL     DLB(YY)       DSEM
064000980202     C                   ELSE
064100980202     C     WEVE          LOOKUP    EVG(YY)                                32
064200980202     C                   MOVEL     DVG(YY)       DSEM
064300980203     C                   MOVEL     §EMDES        V2DCOD
064400980202     C                   END
064500980204     C     *IN32         IFEQ      '0'
064600980202     C                   SETON                                        2844
064700980202     C                   MOVEL     ERR(19)       $MSG
064800980202     C                   END
064900990610     C   28              GOTO      FINCT2
065000980203     C* Se bolla in assegnato no IDD Totale definitivo
065100150728     C*                  Z-ADD     1             X
065200150728     C*    BLPCBO        LOOKUP    C3A(X)                                 32
065300150728     C*                  MOVEL     TPO(X)        WTPO              1
065400150728     C*    *IN32         IFEQ      '1'
065500150728     C*    WTPO          ANDEQ     'A'
065600150728     C*    §EMASS        ANDNE     'S'
065700150728     C*                  SETON                                        2843
065800150728     C*                  MOVEL     ERR(20)       $MSG
065900150728     C*                  END
066000150728     C*  28              GOTO      FINCT2
066100970414     C* Controllo che non sia stata data per partita
066200150728     C*    BLPDT1        IFNE      0
066300150728     C*    §EMTRA        ANDNE     'S'
066400150728     C*                  SETON                                        2843
066500150728     C*                  MOVEL     ERR(13)       $MSG
066600150728     C*                  END
066700150728     C*  28              GOTO      FINCT2
066800970414     C* Controllo che non sia stata trasmessa in sede
066900150728     C*    BLPDT2        IFNE      0
067000150728     C*    §EMTRA        ANDNE     'S'
067100150728     C*                  SETON                                        2843
067200150728     C*                  MOVEL     ERR(14)       $MSG
067300150728     C*                  END
067400150728     C*  28              GOTO      FINCT2
067500970414     C* Controllo che non sia stata trasmessa al cliente
067600150728     C*    BLPDT3        IFNE      0
067700150728     C*    §EMTRA        ANDNE     'S'
067800150728     C*                  SETON                                        2843
067900150728     C*                  MOVEL     ERR(15)       $MSG
068000150728     C*                  END
068100150728     C*  28              GOTO      FINCT2
068200990610      * Controllo C.A.
068300150728     C*    §EMCAC        IFEQ      'S'
068400040715      *
068500150728     c*                  clear                   fidn12ds
068600150728     c*                  eval      i12aas = V2CAAS
068700150728     c*                  eval      i12lnp = V2CLNP
068800150728     c*                  eval      i12nrs = V2CNRS
068900150728     c*                  eval      i12nsp = V2CNSP
069000150728     c*                  eval      i12fel = 'E'
069100150728     c*                  eval      i12fan = 'E'
069200150728     c*                  eval      i12fch = 'E'
069300150728     c*                  call      'FIDN12R'
069400150728     c*                  parm                    fidn12ds
069500040715      **
069600040715      * se c'è almeno una C.A. aperta
069700150728     c*    o12nca        ifgt      0
069800150728     C*                  SETON                                        284331
069900150728     C*                  MOVEL     ERR(24)       $MSG
070000150728     c*                  end
070100040715      **
070200150728     C*                  ENDIF
070300040715      **
070301150728     c                   exsr      ctr_uniE
070400990610     C   28              GOTO      FINCT2
070500040715      **
070600980204     C* Controllo se l'evento è già stato inviato
070700980204     C                   Z-ADD     V2CAAS        KAA2
070800980204     C                   MOVEL     §EMCOD        KCEV
070900980204     C                   MOVEL     ' '           WDAFOR            1
071000990810     C     KEVB          CHAIN     FNEVB04L                           31
071100980204     C* Evento già immesso
071200980204     C     *IN31         IFEQ      '0'
071300980204     C* se non è possibile forzare do errore
071400980204     C     §EMFOR        IFNE      'S'
071500980204     C                   SETON                                        2843
071600980204     C                   MOVEL     ERR(23)       $MSG
071700980204     C                   ELSE
071800980204     C                   MOVEL     'F'           WDAFOR
071900980204     C                   END
072000980204     C                   END
072100990610     C   28              GOTO      FINCT2
072200970414     C*
072300970612     C     FINCT2        ENDSR
072301150724     C*--------------------------------------------------------------*
072302150728     C*    Controlli per il caricamento e gestione sfl
072303150724     C*--------------------------------------------------------------*
072304150728     C     ctr_sfl       BEGSR
072305150724     c                   clear                   $msg
072306150724     c                   setoff                                       28
072307150728     c                   exsr      ctr_uniS
072308150728     c                   if        *in28
072309150728     c                   leavesr
072310150728     c                   endif
072311150728     c                   exsr      ctr_uniE
072312150728     c                   if        *in28
072313150728     c                   leavesr
072314150728     c                   endif
072401150724     c                   clear                   wsispu            1
072410150724     c* controllo che la spedizione non abbia spunte
072411150728     c     kblt          setll     fnblt01l
072412150728     c     kblt          reade     fnblt01l                               99
072413150724    2c                   dow       not *in99
072414150724     c
072415150724     c* Cerco una qualsiasi spunta
072416150724     c     kbrv          chain     fnbrv07l
072417150724    3c                   if        %found(fnbrv07l)
072418150724     c                   eval      wsispu='S'
072419150724     c                   seton                                        99
072420150724   x3c                   else
072421150728     c     kblt          reade     fnblt01l                               99
072422150724    3c                   endif
072423150724    2c                   enddo
072424150724     c*
072425150724    2c                   if        wsispu='S'
072426150724     c                   seton                                        28
072427150724     C                   MOVEL     ERR(29)       $MSG
072428150724     c                   leavesr
072429150724     c                   endif
072447150724     c
072461150724     c
072462150724     c                   endsr
072463150728     C*--------------------------------------------------------------*
072464150728     C*    Controlli unici per spedizione
072465150728     C*--------------------------------------------------------------*
072466150728     C     ctr_uniS      BEGSR
072467150728     C* Controllo se bolla figlia
072468150728     C     KBLP          CHAIN     FNLBL01L
072469150728     C                   if        %found(fnlbl01l)
072470150728     C                   SETON                                        28
072471150728     C                   SETON                                          4344
072472150728     C                   MOVEL     ERR(12)       $MSG
072473150728     c                   leavesr
072474150728     C                   END
072475150728     C* Controllo che non sia stata data per consegnata
072476150728     C     BLPDCM        IFNE      0
072477150728     C     BLPCCA        ANDNE     '6'
072478150728     C     BLPDCM        ORNE      0
072479150728     C     BLPCCA        ANDNE     '2'
072480150728     C                   SETON                                        28
072481150728     C                   SETON                                        43
072482150728     C                   MOVEL     ERR(22)       $MSG
072483150728     c                   leavesr
072484150728     C                   END
072485150728     C* Controllo che abbia il dettaglio segnacolli
072486150728     c     kblt          setll     fnblt01l
072487150728     C                   IF        not %equal(fnblt01l)
072488150728     C                   SETON                                        28
072489150728     C                   SETON                                        43
072490150728     C                   MOVEL     ERR(16)       $MSG
072491150728     c                   leavesr
072492150728     C                   END
072493150728     c                   endsr
072494150728     C*--------------------------------------------------------------*
072495150728     C*    Controlli unici legati all'evento
072496150728     C*--------------------------------------------------------------*
072497150728     C     ctr_uniE      BEGSR
072498150728     C* Se bolla in assegnato no IDD
072499150728     C                   Z-ADD     1             X
072500150728     C     BLPCBO        LOOKUP    C3A(X)                                 32
072501150728     C                   MOVEL     TPO(X)        WTPO              1
072502150728     C     *IN32         IFEQ      '1'
072503150728     C     WTPO          ANDEQ     'A'
072504150728     C     §EMASS        ANDNE     'S'
072505150728     C                   SETON                                        28
072506150728     C                   SETON                                        43
072507150728     C                   MOVEL     ERR(20)       $MSG
072508150728     c                   leavesr
072509150728     C                   END
072519150728     C* Controllo che non sia stata data per partita
072520150728     C     BLPDT1        IFNE      0
072521150728     C     §EMTRA        ANDNE     'S'
072522150728     C                   SETON                                        2843
072523150728     C                   MOVEL     ERR(13)       $MSG
072524150728     c                   leavesr
072525150728     C                   END
072527150728     C* Controllo che non sia stata trasmessa in sede
072528150728     C     BLPDT2        IFNE      0
072529150728     C     §EMTRA        ANDNE     'S'
072530150728     C                   SETON                                        2843
072531150728     C                   MOVEL     ERR(14)       $MSG
072532150728     c                   leavesr
072533150728     C                   END
072535150728     C* Controllo che non sia stata trasmessa al cliente
072536150728     C     BLPDT3        IFNE      0
072537150728     C     §EMTRA        ANDNE     'S'
072538150728     C                   SETON                                        2843
072539150728     C                   MOVEL     ERR(15)       $MSG
072540150728     c                   leavesr
072541150728     C                   END
072543150728      * Controllo C.A.
072544150728      *
072545150728     C     §EMCAC        IFEQ      'S'
072546150728     c                   clear                   fidn12ds
072547150728     c                   eval      i12aas = blpaAS
072548150728     c                   eval      i12lnp = blpLNP
072549150728     c                   eval      i12nrs = blpNRS
072550150728     c                   eval      i12nsp = blpNSP
072551150728     c                   eval      i12fel = 'E'
072552150728     c                   eval      i12fan = 'E'
072553150728     c                   eval      i12fch = 'E'
072554150728     c                   call      'FIDN12R'
072555150728     c                   parm                    fidn12ds
072556150728      **
072557150728      * se c'è almeno una C.A. aperta
072558150728     c     o12nca        ifgt      0
072559150728     C                   SETON                                        28
072560150728     C                   SETON                                          4331
072561150728     C                   MOVEL     ERR(24)       $MSG
072562150728     c                   leavesr
072563150728     c                   end
072564150728     C                   ENDIF
072565150728     c                   endsr
072566970612     C*--------------------------------------------------------------*
072567970612     C* CALPGM: Eseguo Call a pgm
072600970612     C*--------------------------------------------------------------*
072700970612     C     CALPGM        BEGSR
072800970612     C*
072900970612     C*  Imposto DS per richiamo altri pgm
073000970612     C                   CLEAR                   FNLSI1
073001150723     c                   if        wtpvid='S'
073002150723     C                   clear                   DI1FTX
073003150723     C                   MOVEL     V1CKSC        DI1KSC
073004150723     C                   MOVEL     VhdKSC        DI1DEC
073005150723     C                   MOVEL     VshLNP        DI1LNP
073006150723     C                   MOVEL     VshNRS        DI1NRS
073007150723     C                   MOVEL     VshNSP        DI1NSP
073008150723     C                   MOVEL     VshAAS        DI1AAS
073009150723     C                   MOVEL     VshAAS        DI1DSP
073010150723     C                   MOVE      BLPMGS        DI1DSP
073011150723     C                   MOVE      LAB(XY)       DI1LAB
073012150723     C                   MOVE      wsts          DI1STS
073013150723     C                   MOVE      wcev          DI1CEV
073014150723     C                   MOVEL     wsts          WEVE              6
073015150723     C                   MOVE      wcev          WEVE
073016150723
073017150723     C                   Z-ADD     1             YY                5 0
073018150723     C     LAB(XY)       IFEQ      'S'
073019150723     C     WEVE          LOOKUP    ELB(YY)                                32
073020150723     C                   MOVEL     DLB(YY)       DSEM
073021150723     C                   ELSE
073022150723     C     WEVE          LOOKUP    EVG(YY)                                32
073023150723     C                   MOVEL     DVG(YY)       DSEM
073024150723     C                   END
073025150723
073026150723     C                   MOVE      §EMDES        DI1DEV
073027150723     C                   MOVE      §EMDES        DI1DEV
073028150723     C                   MOVE      §EMCOD        DI1C2A
073029150723     C                   MOVE      §EMCCA        DI1CCA
073030150723     C                   MOVE      §EMBLP        DI1BLP
073031150723     C                   MOVE      §EMBLT        DI1BLT
073032150723     C                   MOVE      §EMFOR        DI1FRZ
073033150723     C                   MOVE      §EMTOT        DI1TOT
073034150723     C                   MOVE      BLPNCL        DI1NCL
073035150723     c                   else
073100980203     C                   MOVEL     V2CFTX        DI1FTX
073200980203     C                   MOVEL     V2CKSC        DI1KSC
073300970612     C                   MOVEL     V2DKSC        DI1DEC
073400970612     C                   MOVEL     V2CLNP        DI1LNP
073500970612     C                   MOVEL     V2CNRS        DI1NRS
073600970612     C                   MOVEL     V2CNSP        DI1NSP
073700970612     C                   MOVEL     V2CAAS        DI1AAS
073800970612     C                   MOVEL     V2CAAS        DI1DSP
073900970612     C                   MOVE      BLPMGS        DI1DSP
074000970616     C                   MOVE      BLPMGS        DI1DSP
074100971007     C                   MOVE      LAB(XY)       DI1LAB
074301150723     C                   MOVE      V2CSTS        DI1STS
074302150723     C                   MOVE      V2CCOD        DI1CEV
074400980202     C                   MOVE      §EMDES        DI1DEV
074500980203     C                   MOVE      §EMDES        DI1DEV
074600980203     C                   MOVE      §EMCOD        DI1C2A
074700980203     C                   MOVE      §EMCCA        DI1CCA
074800980203     C                   MOVE      §EMBLP        DI1BLP
074900980203     C                   MOVE      §EMBLT        DI1BLT
075000980203     C                   MOVE      §EMFOR        DI1FRZ
075100980203     C                   MOVE      §EMTOT        DI1TOT
075200980204     C                   MOVE      WDAFOR        DI1FOR
075300980204     C                   MOVE      BLPNCL        DI1NCL
075301150723     c                   endif
075400970612     C                   MOVEL     FNLSI1        KPJBU
075500980202     C* Richiamo pgm gestione dettaglio segnacolli
075600980202     C     §EMTOT        IFNE      'S'
075700980202     C                   CALL      'FNLSI1R2'
075800980202     C                   PARM                    KPJBA
075900980202     C                   MOVEL     KPJBU         FNLSI1
076000980202     C                   END
076100980202     C* Se ho digitato F3 esco
076200980202     C     DI1CMD        IFEQ      '03'
076300980202     C                   MOVEL     'S'           WFINE
076400980202     C                   GOTO      FINRIC
076500980202     C                   END
076600980204     C* Se ho digitato F12 azzero campo scelta
076700980204     C     DI1CMD        IFEQ      '12'
076800980204     C                   GOTO      FINRIC
076900980204     C                   END
077000980202     C* Richiamo pgm gestione terza videata: SPEDIZIONE
077100970612     C                   CALL      'FNLSI1R1'
077200970612     C                   PARM                    KPJBA
077300970617     C                   MOVEL     KPJBU         FNLSI1
077400970612     C*
077500970612     C* Se ho digitato F3 esco
077600970612     C                   SELECT
077700970612     C     DI1CMD        WHENEQ    '03'
077800970619     C                   MOVEL     'S'           WFINE
077900970619     C* Controllo se devo richiamare TRTC81
078000970619     C     WRIC81        IFEQ      'S'
078100970619     C                   EXSR      CALL81
078200970619     C                   END
078300970612     C* Se ho digitato F12 azzero campo scelta
078400970612     C     DI1CMD        WHENEQ    '12'
078500970616     C* CONFERMA
078600970616     C     DI1CMD        WHENEQ    '06'
078700970617     C                   MOVEL     'S'           WRIC81
078800970616     C* Aggiorno BLP/BLT
078900980202     C     §EMBLP        IFEQ      'S'
079000970612     C                   CALL      'FNLSI1R3'
079100970617     C                   PARM                    KPJBA
079200970616     C                   END
079300080421      * l'EDIVAC viene sostituito con il TIVGD Download standard
079400080421     C* Scrivo EDVAC --> TIVGD00F
079500970612     C                   CALL      'FNLSI1R4'
079600970612     C                   PARM                    KPJBA
079700970616     C                   ENDSL
079800970612     C*
079900980202     C     FINRIC        ENDSR
080000970617     C*--------------------------------------------------------------*
080100980126     C* CALL81: PER E.D.I. RICHIAMO PGM TRTC81C
080200970617     C*--------------------------------------------------------------*
080300970617     C     CALL81        BEGSR
080400970617     C*
080500980126      * CALL A TRTC81C PER E.D.I.
080600980126     C     §PTFTT        IFNE      'S'
080700061124     C     §PTFTT        ANDNE     'P'
080800980126     C                   MOVEL     'S'           WCAL81            1
080900080421      *
081000080421      *  Scrive IFTSTA x EDI:
081100080421      * ? il TRTC81C  attiva giro di trasm con SNDIFTSTA  ?
081200080421     C                   MOVEL     *all'0'       Elab_Cliente
081300080421     C                   MOVE      CKS(XX)       Elab_Cliente
081400080421     c                   eval      Tipo_Scarico = CSD(XX)
081500080421      *
081600080421     c                   eval      Tipo_File = 'VC'
081700110630     C                   CALL      'TRTCT81C'
081800080421     c                   parm                    Elab_Cliente      8
081900080422     c                   parm                    Tipo_File         2
082000080421     c                   parm                    Tipo_Scarico      2
082100980126      *
082200980126     C                   END
082300980126      *
082400970617     C                   ENDSR
082500970410     C*--------------------------------------------------------------*
082600970410     C* *INZSR: Operazioni iniziali
082700970410     C*--------------------------------------------------------------*
082800970410     C     *INZSR        BEGSR
082900970410     C*
083000970410     C     *ENTRY        PLIST
083100970410     C                   PARM                    KPJBA
083200990223     C* Richiamo xparut
083300970410     C                   Z-ADD     1             CODUT
083400970410     C                   CALL      'X§PARUT'
083500970410     C                   PARM                    UTEDSE
083600970410     C                   MOVEL     REC80         CNCR80
083700970410     C                   MOVEL     RAGUT         RSUT
083800030924      *
083900030924      *  In base all'utente prende il P.O.
084000030924     C                   CLEAR                   Tibs36Ds
084100030924     c                   EVAL      I36ute = Knmus
084200030924     c                   EVAL      I36Tla = 'L'
084300030924     C                   CALL      'TIBS36R'
084400030924     C                   PARM                    Tibs36Ds
084500030924      *
084600970612     C*--- RICERCA CAPOCONTI------------------------------------------*
084700030116     C                   DO        50            X                 5 0
084800970612     C                   MOVE      TCU(X)        TCUDS
084900970612     C     F56           IFEQ      'CG'
085000970612     C     F4            ANDEQ     '1'
085100970612     C     F3            ANDEQ     '0'
085200970612     C                   Z-ADD     KCU(X)        CLIITA            4 0
085300970612     C                   END
085400970612     C                   END
085500970612     C*--- RICERCA CAPOCONTI------------------------------------------*
085600970410      *  Definizione chiavi
085700970410     C     KBLP          KLIST
085800970410     C                   KFLD                    KAAS
085900970410     C                   KFLD                    KLNP
086000970410     C                   KFLD                    KNRS
086100970410     C                   KFLD                    KNSP
086200970410     C     KBLT          KLIST
086300970410     C                   KFLD                    KAAS
086400970410     C                   KFLD                    KLNP
086500970410     C                   KFLD                    KNRS
086600970410     C                   KFLD                    KNSP
086700970410     C     KLBL1         KLIST
086800970410     C                   KFLD                    KAAP
086900970410     C                   KFLD                    KLPP
087000970410     C                   KFLD                    KNRP
087100970410     C                   KFLD                    KNSP
087200970410     C     KLBL2         KLIST
087300970410     C                   KFLD                    KAAN
087400970410     C                   KFLD                    KLPN
087500970410     C                   KFLD                    KNRN
087600970410     C                   KFLD                    KNSN
087700970410     C     KTAB1         KLIST
087800970410     C                   KFLD                    KKUT
087900970410     C                   KFLD                    KCOD
088000970410     C     KTAB2         KLIST
088100970410     C                   KFLD                    KKUT
088200970410     C                   KFLD                    KCOD
088300970410     C                   KFLD                    KKEY
088400970612     C     KACO          KLIST
088500970612     C                   KFLD                    KKUT
088600970612     C                   KFLD                    KKCC
088700970612     C                   KFLD                    KKSC
088800980204     C     KEVB          KLIST
088900980204     C                   KFLD                    KAA2
089000980204     C                   KFLD                    KLNP
089100980204     C                   KFLD                    KNRS
089200980204     C                   KFLD                    KNSP
089300980204     C                   KFLD                    KCEV
089400080918     C     KTBE1         KLIST
089500080918     C                   KFLD                    TBECOD
089600080918     C                   KFLD                    TBEKE1
089700150415     C     Kedtab        KLIST
089800150415     C                   KFLD                    tabCOD
089900150415     C                   KFLD                    tabKEY
089901150724     C     KBrv          KLIST
089902150724     C                   KFLD                    bltfls
089903150724     C                   KFLD                    bltlna
089904150724     C                   KFLD                    bltNRS
089905150724     C                   KFLD                    bltnsc
090000970410      *  Definizione variabili
090100980204     C     *LIKE         DEFINE    EVBAAS        KAA2
090200980204     C     *LIKE         DEFINE    EVBCEV        KCEV
090300970410     C     *LIKE         DEFINE    BLPAAS        KAAS
090400970410     C     *LIKE         DEFINE    BLPLNP        KLNP
090500970410     C     *LIKE         DEFINE    BLPNRS        KNRS
090600970410     C     *LIKE         DEFINE    BLPNSP        KNSP
090700970410     C     *LIKE         DEFINE    LBLAAP        KAAP
090800970410     C     *LIKE         DEFINE    LBLLPP        KLPP
090900970410     C     *LIKE         DEFINE    LBLNRP        KNRP
091000970410     C     *LIKE         DEFINE    LBLAAN        KAAN
091100970410     C     *LIKE         DEFINE    LBLLPN        KLPN
091200970410     C     *LIKE         DEFINE    LBLNRN        KNRN
091300970410     C     *LIKE         DEFINE    LBLNSN        KNSN
091400970410     C     *LIKE         DEFINE    TBLKUT        KKUT
091500970410     C     *LIKE         DEFINE    TBLCOD        KCOD
091600970410     C     *LIKE         DEFINE    TBLKEY        KKEY
091700970611     C     *LIKE         DEFINE    TABCOD        KCODE
091800970612     C     *LIKE         DEFINE    ACOKCC        KKCC
091900970612     C     *LIKE         DEFINE    ACOKSC        KKSC
092000970410      *
092100970410      *  Reperisco la data del giorno
092200970410     C                   TIME                    WHHDAT           14 0
092300970414     C                   MOVE      WHHDAT        G02DAT
092400970414     C                   MOVEL     *BLANK        G02ERR
092500970414     C                   CALL      'XSRDA8'
092600970414     C                   PARM                    WLBDA8
092700970414     C                   Z-ADD     G02INV        OGGI              8 0
092800970611     C*
092900970611     C* Carico dati DS partner in schiera
093000970612     C                   OPEN      EDTAB01L
093100150415     C                   Z-ADD     0             X
093200970611     C                   MOVEL     'PT'          KCODE
093300970616     C     KCODE         CHAIN     EDTAB01L                           30
093400970611     C     *IN30         DOWEQ     '0'
093500970611     C     TABFLG        IFEQ      ' '
093600970611     C                   ADD       1             X
093700970611     C                   MOVEL     TABUNI        DSPT
093800150415     C                   MOVEL     TABKEY        KEYPT(X)
093900970611     C                   MOVEL     §PTKSC        KSC(X)
094000970616     C                   MOVEL     §PTLNP        LNP(X)
094100970616     C                   MOVEL     §PTNRS        NRS(X)
094200970611     C                   END
094300970616     C     KCODE         READE     EDTAB01L                               30
094400970611     C                   END
094500970616     C*
094600150415     C* x evitare che a causa di un disallineamento imprevisto
094700150415     C*  la PU possa essere disallineata alla PT  eseguo un caricamento
094800150415     C*  differente.
094900150415     c                   clear                   lab
095000150415     c                   do        x             pux               5 0
095100150415     C                   MOVEL     'PU'          tabCOD
095200150415     C                   MOVEL     KEYPT(pux)    tabKEY
095300150415     C     Kedtab        CHAIN     EDTAB01L
095400150415     c                   if        %Found(edtab01l) and tabflg =' '
095500150415      *  Lista Labarta
095600150415     C                   MOVEL     TABUNI        DSPU
095700150415     C                   MOVEL     §PULAB        LAB(pux)
095800150415     c                   end
095900150415     c                   enddo
096000150415      *
096100150415      **** vecchio caricamento
096200150415     C****               Z-ADD     0             X
096300150415     C****               MOVEL     'PU'          KCODE
096400150415     C**** KCODE         CHAIN     EDTAB01L                           30
096500150415     C**** *IN30         DOWEQ     '0'
096600150415     C**** TABFLG        IFEQ      ' '
096700150415     C****               ADD       1             X
096800150415     C****               MOVEL     TABUNI        DSPU
096900150415     C****               MOVEL     §PULAB        LAB(X)
097000150415     C****               END
097100150415     C**** KCODE         READE     EDTAB01L                               30
097200150415     C****               END
097300150415      *
097400150415     C*  Carico in schiera codici collegati - CL -
097500150415     C                   Z-ADD     0             X
097600150415     C                   MOVEL     'CL'          KCODE
097700150415     C     KCODE         CHAIN     EDTAB01L                           30
097800150415     C     *IN30         DOWEQ     '0'
097900150415     C     TABFLG        IFEQ      ' '
098000150415     C                   ADD       1             X
098100150415     C                   MOVEL     TABUNI        DSCL
098200150415     C                   MOVEL     §CLKSC        clKSC(X)
098300150415     C                   MOVEL     §CLLNP        clLNP(X)
098400150415     C                   END
098500150415     C     KCODE         READE     EDTAB01L                               30
098600150415     C                   END
098700980202     C*
098800980202     C* Carico in schiera tabella eventi manuali
098801150723     c                   clear                   wcev
098802150723     c                   clear                   wsts
098900980202     C                   Z-ADD     0             X
099000030116     C                   Z-ADD     0             X1                5 0
099100980202     C                   MOVEL     'EM'          KCODE
099200980202     C     KCODE         CHAIN     EDTAB01L                           30
099300980202     C     *IN30         DOWEQ     '0'
099400980202     C     TABFLG        IFEQ      ' '
099500980202     C                   MOVEL     TABUNI        DSEM
099600980202     C     §EMLAB        IFEQ      'S'
099700980202     C                   ADD       1             X
099800980204     C                   MOVEL     TABKEY        ELB(X)
099900980202     C                   MOVEL     TABUNI        DLB(X)
100000980202     C                   END
100100980202     C     §EMVGL        IFEQ      'S'
100200980202     C                   ADD       1             X1
100300980204     C                   MOVEL     TABKEY        EVG(X1)
100400980202     C                   MOVEL     TABUNI        DVG(X1)
100500980202     C                   END
100501150723     c* memorizzo il codice dell'evento corrispondente alla chiusura bolla
100502150723     c* per merce mai affidata
100503150723     c                   if        §emmai='S'
100504150723     c                   movel     tabkey        wsts
100505150723     c                   eval      wcev=%subst(tabkey:4:3)
100506150723     c                   endif
100600980202     C     KCODE         READE     EDTAB01L                               30
100700980202     C                   END
100800980202     C                   END
100900970611     C* close EDTAB
101000970611     C                   CLOSE     EDTAB01L
101100970611     C*
101200970410     C* Eseguo caricamento tabella 3K
101300970612     C                   OPEN      TABEL00F
101400970410     C                   Z-ADD     0             X
101500970410     C                   Z-ADD     1             KKUT
101600970410     C                   MOVEL     '3K'          KCOD
101700970410     C     KTAB1         CHAIN     TABEL                              30
101800970410     C     *IN30         DOWEQ     '0'
101900030213     C     X             ANDLT     9990
102000970410     C     TBLFLG        IFEQ      *BLANKS
102100970410     C                   ADD       1             X
102200970617     C                   MOVEL     TBLUNI        DS3K
102300970617     C                   MOVEL     TBLKEY        C3K(X)
102400970617     C                   MOVEL     §3KCKS        CKS(X)
102500010725     C                   MOVEL     §3KCSD        CSD(X)
102600970410     C                   END
102700970410     C     KTAB1         READE     TABEL                                  30
102800970410     C                   END
102900970715     C* Eseguo caricamento tabella 3A
103000970721     C                   Z-ADD     0             X
103100970715     C                   MOVEL     '3A'          KCOD
103200970715     C     KTAB1         CHAIN     TABEL                              30
103300970715     C     *IN30         DOWEQ     '0'
103400970721     C     X             ANDLT     500
103500970715     C     TBLFLG        IFEQ      *BLANKS
103600970715     C                   ADD       1             X
103700970715     C                   MOVEL     TBLUNI        DS3A
103800970715     C                   MOVEL     TBLKEY        C3A(X)
103900970715     C                   MOVEL     §3ATB1        TPO(X)
104000970715     C                   END
104100970715     C     KTAB1         READE     TABEL                                  30
104200970715     C                   END
104300970701     C*
104400970701     C* Eseguo caricamento da tabella 7J codici pistole fittizie
104500970701     C                   MOVEA     *HIVAL        C7J
104600970701     C                   Z-ADD     0             X
104700970701     C                   Z-ADD     1             KKUT
104800970701     C                   MOVEL     '7J'          KCOD
104900970701     C     KTAB1         CHAIN     TABEL                              30
105000970701     C     *IN30         DOWEQ     '0'
105100970721     C     X             ANDLT     500
105200970701     C     TBLFLG        IFEQ      *BLANKS
105300970701     C                   MOVEL     TBLUNI        DS7J
105400970912     C     §7JRPS        IFEQ      'A'
105500970701     C                   ADD       1             X
105600970701     C                   MOVEL     TBLKEY        C7J(X)
105700970701     C                   END
105800970701     C                   END
105900970701     C     KTAB1         READE     TABEL                                  30
106000970701     C                   END
106001150728
106002150728     C                   MOVEL     '3I'          Kcod
106003150728     C                   MOVEL     '1       '    kKEY
106004150728     c                   clear                   ds3i
106005150728     C     KTAB2         CHAIN     TABEL00F
106006150728     c                   if        %found(tabel00f)
106007150728     C                   MOVEL     TBLUNI        DS3I
106008150728     c                   endif
106100970410     C* Eseguo caricamento £1
106200020506     c                   clear                   trul06ds
106300020506     c                   eval      d06cod = '£1'
106400030924     c                   movel     o36pou        d06key
106500020506     c                   movel     trul06ds      kpjbu
106600020506     c                   call      'TRUL06R'
106700020506     c                   parm                    kpjba
106800020506     c                   movel     kpjbu         trul06ds
106900020506     c                   movea     lin           £1
106901150724     c* terminal di partenza del p.o. utente
106902150724     c                   clear                   fnlv55ds
106903150724     C                   MOVEL     'P'           D55TPT
106904150724     C                   MOVEL     simpou        D55LIN
106905150724     C                   z-add     oggi          D55drf
106906150724     C                   CALL      'FNLV55R'
106907150724     C                   PARM                    fnlv55ds
106908150724     c                   z-add     d55tfp        wtfp
107000020912      *
107100971008     C* Imposto a 'N' flag richiamo TRTC81C
107200971008     C                   MOVEL     'N'           WCAL81            1
107300970410     C* Eseguo close del file
107400970410     C                   CLOSE     TABEL00F
107500970410     C*
107600970410     C                   ENDSR
107700080930      *------------------------------------------------------------------------*
107800080930      * Routine di controllo x eventuali Errori
107900080930      *------------------------------------------------------------------------*
108000080930     C     *PSSR         BEGSR
108100080930      *
108200080930      * Comunque eseguo x rilasciare il TIVGD.
108300080930      *? Infine elimino il blocco elaborazione TIVGD x tipo file qui.  ?
108400080930     C                   clear                   trul47ds
108500080930     C                   eval      d47opz  = 'F'
108600080930     C                   eval      d47tip  = 'VC'
108700080930     C                   call      'TRUL47R'
108800080930     C                   parm                    trul47ds
108900080930      *
109000080930     C                   ENDSR     '*CANCL'
109100080930     C*------------------------------------------------------------------------*
109200971008**
109300970701Immettere codice cliente                                                      01
109400970701Codice cliente inesistente                                                    02
109500060811Codice cliente non presente in tabella 3K o PT                                03
109600970701Un altro utente sta chiudendo le spedizioni del cliente                       04
109700970701Immettere anno della spedizione                                               05
109800970701Immettere linea di partenza della spedizione                                  06
109900970701Linea di partenza inesistente                                                 07
110000970701Linea di partenza non gestita su questo as                                    08
110100970701Immettere numero della spedizione                                             09
110200970701Spedizione inesistente                                                        10
110300980206Bolla mamma: impossibile gestirla                                             11
110400980206Bolla figlia: impossibile gestirla                                            12
110500970701Spedizione già trasmessa alla filiale di arrivo                               13
110600970701Spedizione già trasmessa alla sede                                            14
110700970701Spedizione già trasmessa al cliente                                           15
110800980202Spedizione senza colli: impossibile eseguire chiusura                         16
110900980202Spedizione non appartenente al cliente selezionato                            17
111000980202Il codice selezionato deve appartenere ad un partner                          18
111100980202Codice evento inesistente                                                     19
111200980202Bolla in assegnato: chiusura definitiva possibile solo in arrivo              20
111300980202Codice evento obbligatorio                                                    21
111400980204Spedizione già consegnata                                                     22
111500980204Evento già inviato impossibile rinviarlo                                      23
111600990610Esistono C.A. aperte per la spedizione, codice evento non valido              24
111700131016 ATTENZIONE: più bolle con stesso riferimento!!                               25
111800131016 VERIFICARE se la Spedizione è quella corretta.                               26
111900150723Nessun dato da visualizzare per la selezione richiesta                        27
111901150728                                                                              28
112100150724Spedizione non gestibile:presenti spunte                                      29
112200150728Attenzione: caricate solo 9.999 bolle ma ne sono presenti ancora              30
112300150909Non trovato Evento per Chiusura per Merce mai Affidata. Contattare il CED (EM)31
