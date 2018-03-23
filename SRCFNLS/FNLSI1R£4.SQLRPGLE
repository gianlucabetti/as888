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
001200150724     FFNBrv07L  IF   E           K DISK
001300970410     F*--------
001400970410     FFNLBL01L  IF   E           K DISK
001500970410     F*--------
001600970410     FFNLBL02L  IF   E           K DISK
001700970611     F                                     RENAME(FNLBL000:FNLBL2)
001800970611     F*--------
001900130822     FFNBLp32L  IF   E           K DISK
002000130822     F                                     RENAME(FNBLP000:FNBLP32)
002100130822     F*--------
002200130822     FFNBLp33L  IF   E           K DISK
002300130822     F                                     RENAME(FNBLP000:FNBLP33)
002400130822     F*--------
002500970611     FCNACO00F  IF   E           K DISK
002600990610     F*--------
002700040715     F****FNDCT02L  IF   E           K DISK
002800970410     F*--------
002900970410     FTABEL00F  IF   E           K DISK    USROPN
003000970611     F*--------
003100970611     FEDTAB01L  IF   E           K DISK    USROPN
003200970410     F*--------
003300150724     FAZORG01L  IF   E           k disk
003400970414     F*--------
003500970414     FFNLSI1D   CF   E             WORKSTN
003600150723     f                                     sfile(lsi1S01 : S01nrr)
003700080918     F*--------
003800080918     FTNTBE01L  IF   E           K DISK
003900970410     D*--------------------------------------------------------------*
004000970410     D*  SCHIERE
004100970410     D*--------------------------------------------------------------*
004200970612     D*  Caricamento capoconto clienti
004300970410     D* Schiera codici tabella 3K: invio date consegna
004400030108     D C3K             S              7  0 DIM(9990)
004500030108     D CKS             S              7  0 DIM(9990)
004600030108     D CSD             S              2    DIM(9990)
004700970715     D* Tipi bolla: tabella 3A
004800970715     D C3A             S              2    DIM(500)
004900970715     D TPO             S              2    DIM(500)
005000970701     D* Schiera codici pistole fittizie: 7J
005100970701     D C7J             S              2  0 DIM(500)
005200970611     D* Schiera codici tabella PT
005300150415     D KEYPT           S             35    DIM(500)
005400970611     D KSC             S              7  0 DIM(500)
005500970616     D LNP             S              3  0 DIM(500)
005600970616     D NRS             S              2  0 DIM(500)
005700970616     D LAB             S              1    DIM(500)
005800150415     D* Schiera codici tabella CL
005900150415     D clKSC           S              7  0 DIM(999)
006000150415     D clLNP           S              3  0 DIM(999)
006100980202     D* Schiera codici tabella EM
006200980202     D ELB             S              6    DIM(500)
006300980202     D DLB             S             90    DIM(500)
006400980202     D EVG             S              6    DIM(500)
006500980202     D DVG             S             90    DIM(500)
006600970410     D* Caricamento £1
006700020506     D*UNI             S              3    DIM(29)
006800020506     D £1              S              3  0 DIM(30)
006900970414     D* ERRORI
007000150909     D ERR             S             78    DIM(31) CTDATA PERRCD(1)
007100150723
007200150723     d s01rcd          s                   like(c01rcd)
007300150723     d $Inzs01         s               n   inz(*on)
007400150723     d $Finerec        s               n   inz(*off)
007500150723     d wcev            s                   like(v2ccod)
007600150723     d wsts            s                   like(v2csts)
007700150727     d S01nrr          s              7  0 inz
007800150723     d Datadmy         s               d   datfmt(*dmy)
007900150724     d wtfp            s                   LIKE(BLPTFP)
008000150723     D WrkStringaSql   S           4500
008100150723     D                                     VARYING
008200970410     D*--------------------------------------------------------------*
008300970410     D* DS
008400970410     D*--------------------------------------------------------------*
008500030924     D Tibs36ds      E DS
008600970612     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
008700970612     D  TCU                  398    697
008800970612     D                                     DIM(50)
008900970612     D  KCU                  698    847P 0
009000970612     D                                     DIM(50)
009100970612     D                                     PACKEVEN
009200970612     D TCUDS           DS
009300970612     D  F3                     3      3
009400970612     D  F4                     4      4
009500970612     D  F56                    5      6
009600970612     D CNCR80        E DS
009700020506     d trul06ds      e ds
009800020506     d  lin                    1     90  0 dim(30)
009900970612     D* DS  appoggio x cmd
010000970616     D CMDAPP          DS            50
010100970616     D  NOMMBR                42     48
010200970617     D CMDLOG          DS            80
010300970617     D  MBRLOG                42     48
010400970617     D  MBRFIS                71     77
010500150724     d fnlv55ds      e ds
010600970414     D WLBDA8          DS
010700970414     D  G02DAT                 1      8  0
010800970414     D  G02INV                 9     16  0
010900970414     D  G02ERR                17     17
011000970414     D  G02TGI                18     22  0
011100150728     d WDAT8           DS
011200150728     d  datada                 1      8  0
011300150728     d  dataa                  9     16  0
011400150728     d  ggl                   17     21  0
011500080421      *----------------------------------------------------------------
011600080421      * Controllo e semaforo x scrittura del VAC: TRUL47R
011700080421      *----------------------------------------------------------------
011800080421     D trul47ds      e ds
011900080421      *
012000970410     D KPJBA         E DS
012100150723     d fnblpds       e ds                  extname(fnblp00f) inz
012200970612     D DSPT          E DS                  EXTNAME(EDIDSPT)
012300970616     D DSPU          E DS                  EXTNAME(EDIDSPU)
012400980202     D DSEM          E DS                  EXTNAME(EDIDSEM)
012500160209     D s_dsem        E DS                  EXTNAME(EDIDSEM)
012600160209     D                                     prefix(S_)
012700150415     D DSCL          E DS                  EXTNAME(EDIDSCL)
012800970616     D* DATI X RICHIAMO PGM INTERR. BOLLE PARTENZE (FNLS05R)
012900970616     D PARA04          DS                  INZ
013000970616     D  P04AAS                 1      4  0
013100970616     D  P04LNP                 5      7  0
013200970616     D  P04NRS                 8      9  0
013300970616     D  P04NSP                10     16  0
013400980204     D  P04F03                17     17
013500970616     D  P04FLG                31     31
013600970616     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
013700970616     D  P04GIA                48     48
013800970616      * PA3CLI indica se il pgm e' richiamato da un cliente (='S')
013900970616     D  P04CLI               143    143
014000970616     D PARA05          DS                  INZ
014100970616     D  P05AAS                 1      4  0
014200970616     D  P05LNP                 5      7  0
014300970616     D  P05NRS                 8      9  0
014400970616     D  P05NSP                10     16  0
014500970616     D  P05F03                17     17
014600970616     D  P05FLG                18     18
014700970616     D  P05SIM                44     46  0
014800970616     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
014900970616     D  P05GIA                48     48
015000080421     D***** TIBS55        E DS                  EXTNAME(TIBS55DS)
015100971126     D FNLSI1        E DS                  EXTNAME(FNLSI1DS)
015200970410     D DS3K          E DS
015300970715     D DS3A          E DS
015400970701     D DS7J          E DS
015500970410     D DS7O          E DS
015600150728     D DS3i          E DS
015700060811     D OG143         E DS
015800040715
015900040715     d Fidn12ds      e ds
016000040715     d  skKey                 26    305    dim(20)
016100040715     d  skAnn                306    325    dim(20)
016200040715     d  skDca                326    485    dim(20)
016300040715     d  skFca                486    545  0 dim(20)
016400040715     d  skDch                546    705  0 dim(20)
016500040715     d  skCch                706    745    dim(20)
016600040715
016700970410     D                SDS
016800970410     D  NOMPGM                 1     10
016900970410     I*--------------------------------------------------------------*
017000970410     I*  Definizione campi Azorg
017100970410     I*--------------------------------------------------------------*
017200150724     I***AZORG01L  AA
017300150724     I***                                  1    3  ORGDIT
017400150724     I***                             P    4    5 0ORGFIL
017500150724     I***                               3776 3800  ORGDE3
017600150728      /copy gaitrasrc/srcprotopr,xsrlav8
017700970410     C*--------------------------------------------------------------*
017800970410     C*  C I C L O     P R I N C I P A L E
017900970410     C*--------------------------------------------------------------*
018000150723      /free
018100150723         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
018200150723      /end-free
018300970410     C*  Inizializzo varibili videata
018400970410     C                   EXSR      INZ01
018500080421      *
018600970410     C*  Gestione della prima videata
018700970410     C     WFINE         DOWEQ     'N'
018800970611     C     WTPVID        CASEQ     '1'           GESD01
018900970611     C     WTPVID        CASEQ     '2'           GESD02
019000150723     C     WTPVID        CASEQ     'S'           GESsfl
019100970410     C                   END
019200970611     C                   END
019300970410     C*  Fine pgm
019400970410     C                   SETON                                        LR
019500970611     C*--------------------------------------------------------------*
019600970611     C* INZ01: Impostazione della prima videata
019700970611     C*--------------------------------------------------------------*
019800970611     C     INZ01         BEGSR
019900970611     C*
020000970611     C                   SETOFF                                       2840
020100970616     C                   MOVEL     '1'           WTPVID
020200970617     C                   MOVEL     'N'           WRIC81            1
020300970616     C                   MOVEL     'N'           WFINE
020400970611     C                   MOVEL     NOMPGM        V1CPGM
020500970611     C                   Z-ADD     0             V1CKSC
020600970611     C*
020700970611     C                   ENDSR
020800970410     C*--------------------------------------------------------------*
020900970611     C* INZ02: Impostazione della prima videata
021000970410     C*--------------------------------------------------------------*
021100970611     C     INZ02         BEGSR
021200970410     C*
021300970410     C                   SETOFF                                       284041
021400970611     C                   SETOFF                                       424344
021500970616     C                   MOVEL     '2'           WTPVID
021600970611     C                   MOVEL     NOMPGM        V2CPGM
021700970611     C                   MOVEL     V1CKSC        V2CKSC
021800970611     C                   MOVEL     ACORAG        V2DKSC
021900980204     C                   MOVEL     *BLANKS       V2DCOD
022000980204     C                   MOVEL     *BLANKS       V2CCOD
022100980202     C                   MOVEL     *BLANKS       V2CSTS
022200980202     C                   MOVEL     *BLANKS       V2CFTX
022300970611     C                   MOVE      WHHDAT        V2CAAS
022400130304      **  gestione cliente con Partner gestito da altra filiale
022500130304      **  vedi GWEISS con clienti sulla 277.
022600130304     c                   if        LNP_ALTRA_FIL = 0
022700971007     C                   Z-ADD     LNP(XY)       V2CLNP
022800130821      ******
022900130821     c**** Chiodo x Ducros da Eliminare fra 333 e 330
023000130821     c                   if        v2clnp =333
023100130821     c                   eval      v2clnp =330
023200130821     c                   end
023300130821      ******
023400130304     c                   else
023500130304     C                   eval        V2CLNP = LNP_ALTRA_FIL
023600130304     c                   end
023700971007     C                   Z-ADD     NRS(XY)       V2CNRS
023800970611     C                   Z-ADD     0             V2CNSP
023900970410     C*
024000970410     C                   ENDSR
024100970410     C*--------------------------------------------------------------*
024200970410     C* GESD01: Gestione della prima videata
024300970410     C*--------------------------------------------------------------*
024400970410     C     GESD01        BEGSR
024500970410     C*
024600970410     C* Eseguo exfmt della prima videata
024700970410     C                   EXFMT     LSI1D01
024800970611     C                   SETOFF                                       2840
024900970410     C                   SETOFF                                       4243
025000970410     C* Fine
025100970410     C     *INKC         IFEQ      '1'
025200970414     C                   MOVEL     'S'           WFINE             1
025300970410     C                   GOTO      FINVD1
025400970410     C                   END
025500970410     C* Controlli
025600970410     C                   EXSR      CTR01
025700970410     C   28              GOTO      FINVD1
025800970611     C*
025900970611     C* Se no errori passo a videata successiva
026000150723     c* chiusura bolle per merce mai affidata --> la videata successiva è il sfl
026100150723     c* delle bolle
026200150723     c                   if        v1cfbo='S'
026300150723     C                   MOVEL     'S'           WTPVID
026400150723     c                   eval      $inzS01=*on
026500150723     c                   else
026600150724     C                   EXSR      INZ02
026700150723     C                   MOVEL     '2'           WTPVID
026800150723     c                   endif
026900970410     C*
027000970410     C     FINVD1        ENDSR
027100970611     C*--------------------------------------------------------------*
027200970611     C* GESD02: Gestione della seconda videata
027300970611     C*--------------------------------------------------------------*
027400970611     C     GESD02        BEGSR
027500970611     C*
027600970611     C* Eseguo exfmt della prima videata
027700970611     C                   EXFMT     LSI1D02
027800970611     C                   SETOFF                                       284041
027900970611     C                   SETOFF                                       424344
028000970611     C* Fine
028100970611     C     *INKC         IFEQ      '1'
028200970611     C                   MOVEL     'S'           WFINE             1
028300970617     C* Controllo se devo richiamare TRTC81
028400970617     C     WRIC81        IFEQ      'S'
028500970617     C                   EXSR      CALL81
028600970617     C                   END
028700970611     C                   GOTO      FINVD2
028800970611     C                   END
028900970611     C* Prima videata
029000970611     C     *INKL         IFEQ      '1'
029100970611     C                   MOVEL     '1'           WTPVID            1
029200970617     C* Controllo se devo richiamare TRTC81
029300970617     C     WRIC81        IFEQ      'S'
029400970617     C                   EXSR      CALL81
029500970617     C                   END
029600970611     C                   GOTO      FINVD2
029700970611     C                   END
029800080421      *
029900970612     C* Interrogazione bolle
030000970612     C     *INKG         IFEQ      '1'
030100970616     C     V2CNSP        IFGT      0
030200970616     C                   CLEAR                   PARA05
030300970616     C                   Z-ADD     V2CAAS        P05AAS
030400970616     C                   Z-ADD     V2CLNP        P05LNP
030500970616     C                   Z-ADD     V2CNRS        P05NRS
030600970616     C                   Z-ADD     V2CNSP        P05NSP
030700970616     C                   MOVEL     '1'           P05GIA
030800970616     C                   MOVEL     PARA05        KPJBU
030900970616     C                   CALL      'FNLS05R'
031000970616     C                   PARM                    KPJBA
031100970616     C                   MOVEL     KPJBU         PARA05
031200980204     C     P05F03        IFEQ      *BLANKS
031300980204     C                   Z-ADD     P05AAS        V2CAAS
031400980204     C                   Z-ADD     P05LNP        V2CLNP
031500980204     C                   Z-ADD     P05NRS        V2CNRS
031600980204     C                   Z-ADD     P05NSP        V2CNSP
031700980204     C                   ENDIF
031800970616     C*
031900970616     C                   ELSE
032000970616     C*
032100970616     C                   CLEAR                   PARA04
032200970616     C                   MOVEL     '1'           P04FLG
032300970616     C                   MOVEL     PARA04        KPJBU
032400970616     C                   CALL      'FNLS04R'
032500970616     C                   PARM                    KPJBA
032600980204     C                   MOVEL     KPJBU         PARA04
032700980204     C     P04F03        IFEQ      *BLANKS
032800980204     C                   Z-ADD     P04AAS        V2CAAS
032900980204     C                   Z-ADD     P04LNP        V2CLNP
033000980204     C                   Z-ADD     P04NRS        V2CNRS
033100980204     C                   Z-ADD     P04NSP        V2CNSP
033200970616     C                   ENDIF
033300980204     C                   ENDIF
033400970612     C                   GOTO      FINVD2
033500970612     C                   END
033600970611     C* Controlli
033700970611     C                   EXSR      CTR02
033800970611     C   28              GOTO      FINVD2
033900970611     C*
034000970611     C* Se no errori vado a pgm successivo
034100970611     C                   EXSR      CALPGM
034200970616     C* Azzero prima videata se ho eseguito conferma
034300970616     C     DI1CMD        IFEQ      '06'
034400970617     C                   EXSR      INZ02
034500970616     C                   END
034600970611     C*
034700970611     C     FINVD2        ENDSR
034800970611     C*--------------------------------------------------------------*
034900970611     C* CTR01: Controlli prima videata
035000970611     C*--------------------------------------------------------------*
035100970611     C     CTR01         BEGSR
035200970611     C*
035300970611     C* Controllo se immesso codice cliente
035400970611     C     V1CKSC        IFEQ      0
035500970611     C                   SETON                                        2840
035600970611     C                   MOVEL     ERR(1)        $MSG
035700970611     C                   END
035800990610     C   28              GOTO      FINCT1
035900150415      *
036000970611     C* Controllo se esiste cliente
036100970612     C                   Z-ADD     1             KKUT
036200970611     C                   Z-ADD     CLIITA        KKCC
036300970611     C                   Z-ADD     V1CKSC        KKSC
036400970611     C     KACO          CHAIN     CNACO00F                           31
036500970611     C     *IN31         IFEQ      '1'
036600150723     c                   clear                   vhdksc
036700970611     C                   SETON                                        2840
036800970611     C                   MOVEL     ERR(2)        $MSG
036900970611     C                   END
037000990610     C   28              GOTO      FINCT1
037100150723     c                   movel     acorag        vhdksc
037200150415      ****
037300970617     C* Controllo Codice cliente unificante tabella 3K
037400030116     C                   Z-ADD     1             XX                5 0
037500970617     C     V1CKSC        LOOKUP    C3K(XX)                                32
037600060811      *
037700060811      **?se non ha agganciato nulla
037800060811     c                   if        *in32 = *off
037900150415      *
038000060811     c     v1cksc        div       10000         trelnp            3 0
038100060811     C                   Z-ADD     treLNP        ORGFIL
038200060811     C     ORGFIL        CHAIN     AZORG01L                           31
038300060811      **? e se  si tratta di DPD (eccezione .......)
038400060811     c                   if        %Found(AZORG01L)
038500060811     c                   movel     orgde3        og143
038600060811     c                   if        §OGNTW ='DPD'
038700060811     C                   Z-ADD     1             XY                5 0
038800150415      ** va direttamente sulla PT poichè DPD non sta sulla 3K
038900060811     C     V1CKSC        LOOKUP    KSC(XY)                                32
039000060811     c                   end
039100060811     c                   end
039200060811      **
039300060811     c                   else
039400060811      **
039500150415      **  con l'unificante della 3K
039600970617     C* Controllo se cliente in tabella PT
039700060811     C                   Z-ADD     1             XY                5 0
039800060811     C     CKS(XX)       LOOKUP    KSC(XY)                                32
039900150415      **
040000060811     c                   end
040100060811      **
040200150415      **  Se non l'avesse trovato con l'UNIFICANTE sulla PT
040300970611     C     *IN32         IFEQ      '0'
040400150415      **
040500970611     C                   SETON                                        2840
040600970611     C                   MOVEL     ERR(3)        $MSG
040700150415      **
040800970617     C                   ELSE
040900020912      *
041000020912      *  la Linea di Partenza del Partner deve essere gestita
041100020912      *   nella £1
041200020912     C     LNP(XY)       lookup    £1                                     32
041300080918      ** ----
041400130304      **     Potrebbe essere un cliente appartenente alla tabella CL
041500130304      **     gestito da un'altra filiale. a questo punto occorre controllare,
041600130304      **     non solo la LINEA del PARTNER agganciata sull'unificante della 3K
041700130304      **     ma se non fosse gestito il Partner sulla filiale si deve vedere
041800130304      **     la Linea del Cliente se è gestita sulla filiale.
041900130304      **   (Esempio su clienti di GWeiss della 360 dati in gestione alla 277.
042000130304      **    il cliente è il 2772159 unificato sulla 3K al 3600001.
042100130304      **   Non è gestita la 360 nella 277 allora il cliente 2772159 è della 277
042200130304      **    quindi deve passare il controllo...poichè il cliente è in gestione)
042300130304     c                   clear                   LNP_ALTRA_FIL     3 0
042400150415      **
042500130304     c                   if        NOT *in32
042600150415      **
042700150415      ** Dovendo gestire casi in cui il cliente è stato spostato ANCORA di
042800150415      **  Filiale mantenendo il codice cliente, occorre effettuare il
042900150415      ** ragionamento sulla LNP presente sulla tab.CL o sulla PT per
043000150415      ** verificare l'appartenenza alla £1 e NON con i primi 3 caratteri del
043100150415      ** codice cliente.
043200150415      ** E'sempre il caso della GWEISS che adesso da 277 viene spostato alla
043300150415      **  249 mantenendo il codice cliente come 277xxxx e facendo capo alla 360
043400150415      **
043500150415     c                   clear                   treLNP            3 0
043600150415     c**** v1cksc        div       10000         treLNP            3 0
043700150415      ** ---
043800150415      **  cerca sulla CL  x impostare la LNP
043900150415     C                   Z-ADD     1             clpt              5 0
044000150415     C     V1CKSC        LOOKUP    clKSC(clpt)                            32
044100150415     c                   if        *in32
044200150415     c                   z-add     clLNP(clpt)   treLNP
044300150415     c                   else
044400150415      **  se non la trova cerca sulla PT  x impostare la LNP
044500150415     C                   Z-ADD     1             clpt              5 0
044600150415     C     V1CKSC        LOOKUP    KSC(clpt)                              32
044700150415     c   32              z-add     LNP(clpt)     treLNP
044800150415     c                   end
044900150415      ** ---
045000150415      **
045100130304     C     treLNP        lookup    £1                                     32
045200130304     c   32              movel     treLNP        LNP_ALTRA_FIL
045300150415      **
045400130304     c                   end
045500130304      ** ----
045600080918      ** Occorre anche controllare se DPD di backup quindi linee collegate
045700080918      **  per potere gestire linee da filiali diverse
045800080918      **   ossia tab."3NN"
045900080918      **
046000080918     C     *IN32         ifeq      *off
046100080918     c                   movel(p)  '3NN'         TbeCOD
046200080918     c                   movel(p)  KSC(xy)       TbeKE1
046300080918     c     kTBE1         setll     tntbe01l
046400080918     c     kTBE1         reade     tntbe01l
046500080918     C                   dow       not %EoF(tntbe01l)
046600080918     c                   movel(p)  tbeKE2        LNPAlternativa    3 0
046700080918     C     LNPAlternativalookup    £1                                     32
046800080918     c                   if        *in32
046900080918     c                   z-add     1             z                 3 0
047000080918     c     *zero         lookup    £1(z)                                  21
047100080918      * aggiunge la linea x il controllo successivo
047200080918     c                   if        *in21
047300080918     C                   z-add     lnp(xy)       £1(z)
047400080918     C                   End
047500080918     C                   Leave
047600080918     C                   End
047700080918     c     kTBE1         reade     tntbe01l
047800080918     C                   End
047900080918     C                   End
048000080918      ** ----
048100020912     C     *IN32         ifeq      *off
048200020912     C                   SETON                                        2840
048300020912     C                   MOVEL     err(8)        $MSG
048400020912     C                   Else
048500020912     C*
048600020912     C                   EndIF
048700020912     C*
048800970617     C                   END
048900020912     C*
049000990610     C   28              GOTO      FINCT1
049100150909     c* Se richiesta visualizzazione bolle da chiudere per merce mai
049200150909     c* affidata ERRORE se non trovato l'evento su tabella EM
049300150909     c                   if        v1cfbo='S' and wsts=*blanks
049400150909     C                   SETON                                        28
049500150909     C                   MOVEL     err(31)       $MSG
049600150909     c                   endif
049700080421     C*
049800970611     C     FINCT1        ENDSR
049900150723     C*--------------------------------------------------------------*
050000150723     C* gessfl:  videata sfl per codice cliente
050100150723     C*--------------------------------------------------------------*
050200150723     C     gessfl        BEGSR
050300150723      /free
050400160209         // Valorizzo DSEM con i dati dell'evento di chisura per merce mai aff.
050500160209         dsem=s_dsem ;
050600160209
050700150723         if $inzs01=*on;
050800150723            exsr inzs01;
050900150723            c01csr=1;
051000150723            c01rcd=1;
051100150723            $inzs01=*off;
051200150723         // Se non trovati record errore in prima videata
051300150723            if s01nrr=0   ;
051400150723               $msg=err(27)  ;
051500150723               *in28=*on  ;
051600150723               *in40=*on  ;
051700150723               wtpvid='1'  ;
051800150723               leavesr   ;
051900150723            endif  ;
052000150723            if s01nrr=1;
052100150724              chain (vshaas:vshlnp:vshnrs:vshnsp) fnblp01l ;
052200150723              exsr calpgm;
052300150723              leavesr;
052400150723            endif;
052500150723         endif;
052600150723       // c01csr   contiene il numero di riga del subfile su cui era posizionato
052700150723       // impostando c01rcd   visualizzo la pagina che vedeva l'utente quando ha
052800150723       // l'ultimo tasto
0529001507232         if c01csr   > 0;
053000150723           c01rcd   = c01csr;
053100150723          else;
053200150723           c01rcd   = s01rcd   ;
0533001507232         endif;
053400150723       // se non so quale pagina visualizzare forzo pagina 1
0535001507232         if c01rcd   < 1;
053600150723           eval c01rcd   = 1;
0537001507232         endif;
053800150723
053900150723       // valorizzo campi del sfl control
054000150723          vc1ksc=%int(v1cksc);
054100150724          vc1kscd=vhdksc;
054200150723
054300150723
054400150723         // Emissione videata
054500150723         write  lsi1z01;
054600150723         exfmt  lsi1C01;
054700150723
054800150723         s01nrr=1  ;
054900150723
055000150723 1       SELECT;
055100150723
055200150723         // - F3=Fine
055300150723 1         WHEN  *inkc;
055400150723               wFine = 'S';
055500150723
055600150723         // - F12=Ritorno
055700150723 1         WHEN  *inkl;
055800150914               *in28=*off;
055900150723               wtpvid='1'  ;
056000150723
056100150723
056200150723         // -
056300150723 1       //WHEN  *inkb ;
056400150723         //  exsr seltot;
056500150723
056600150723
056700150723 x1      // Invio
056800150723           OTHER;
056900150723
057000150723               exsr ContrS01 ;
057100150723
057200150723 1       ENDSL;
057300150723      /end-free
057400150723     c                   endsr
057500150723      /free
057600150723       //--------------------------------------------------------------
057700150723       BEGSR InzS01;
057800150723
057900150723       // Pulizia subfile
058000150723         *in30  = *on;
058100150723         *in31  = *on;
058200150723         write  lsi1c01;
058300150723         *in31  = *off;
058400150723         *in30  = *off;
058500150723         *in32  = *off;
058600150723
058700150723         exec sql close BLPcsr;
058800150723       // Richiamo routine di preparazione stringa SQL
058900150723             exsr sr_prepsql;
059000150723
059100150723         S01nrr=0 ;
059200150723         clear $msg;
059300150723         *in28  = *off;
059400150723         *in90  = *off;
059500150728         *in19=*off;
059600150723       // Elaboro i records estratti
059700150723           $finerec=*off;
059800150723           exec sql prepare STRINGASQL from :WrkStringaSql;
059900150723           exec sql declare BLPCSR cursor for StringaSql;
060000150723           exec sql open BLPcsr;
060100150723           dow $finerec=*off;
060200150723              exec sql Fetch BLPcsr into :fnblpds;
060300150728              *in28  = *off;
060400150728              if sqlcod=100 or sqlcod<0;
060500150723                 $finerec = *on;
060600150723                 leave;
060700150723              endif;
060800150728              if s01nrr=9999;
060900150728                 $finerec = *on;
061000150728                 $msg=err(30);
061100150728                 *in28=*on;
061200150728                 leave;
061300150728              endif;
061400150723              if %lookup(blplnp:£1)>0;
061500150724                 kaas=blpaas;
061600150724                 klnp=blplnp;
061700150724                 knrs=blpnrs;
061800150724                 knsp=blpnsp;
061900150728       //per poter chiudere la spedizione come merce mai affidata
062000150728       //devono essere trascorsi i giorni max di sospensione previsti
062100150728       //da tabella
062200150728                 clear wdat8;
062300150728                 datada=blpdim;
062400150728                 dataa=(blpaas*10000)+blpmgs;
062500150728                 xsrlav8(wdat8);
062600150728                 if (ggl-1) >  §3iggc;
062700150728                    exsr ctr_sfl;
062800150728                    if not *in28;
062900150728           // Carico record nel sfl
063000150728                       exsr carsf01;
063100150728                    endif;
063200150728                 endif;
063300150723              endif;
063400150723           enddo;
063500150723
063600150723          s01rcd    = s01nrr  ;
063700150723
063800150723         ENDSR  ;
063900150723       //--------------------------------------------------------------
064000150723       //?Preparazione stringa SQL
064100150723       //--------------------------------------------------------------
064200150723       BEGSR sr_prepsql;
064300150723       WrkStringaSql='select * from fnblp00f' +
064400150724       ' where (blpksc=' + %editc(v1cksc:'X') + ' or '      +
064500150724       ' blpccm=' + %editc(v1cksc:'X') +
064600150724       ') and blptfp=' + %editc(wtfp:'X') +
064700150723       ' and blpnfv=0' +
064800150723       ' order by  blptfp, blpnfv, blpdim';
064900150723       endsr;
065000150723       //--------------------------------------------------------------
065100150723       //?eseguo caricamento SFL
065200150723       //--------------------------------------------------------------
065300150723          BEGSR   carSF01    ;
065400150723          clear vs1opz  ;
065500150723
065600150723          exsr CarRECORD  ;
065700150723          vshaas=blpaas;
065800150723          vshlnp=blplnp;
065900150723          vshnrs=blpNRs;
066000150723          vshnsp=blpNsp;
066100150723          s01nrr=s01nrr+1   ;
066200150723          write  lsi1S01;
066300150723
066400150723          ENDSR         ;
066500150723       //--------------------------------------------------------------
066600150723       //?Caricamento record SFL
066700150723       //--------------------------------------------------------------
066800150723       BEGSR CarRecord;
066900150723       // data Immissione bolla
067000150723             clear vs1dim;
067100150723             datadmy=%date(blpdim:*iso);
067200150723             vs1dim=%dec(datadmy);
067300150723       // Data Spedizione
067400150723             clear vs1dsp;
067500150723             eval datadmy=%date((blpaas*10000+blpmgs):*iso);
067600150723             vs1dsp=%dec(datadmy);
067700150723       // linea di arrivo
067800150723             vs1lna=blplna;
067900150723             chain blplna azorg01l ;
068000150723             if orgde5<>*blanks;
068100150723                 vs1lnad=%subst(orgde5:1:10);
068200150723             else;
068300150723                 vs1lnad=%subst(orgdes:1:10);
068400150723             endif;
068500150723       // Destinatario
068600150723             vs1rsd=blprsd;
068700150723       ENDSR         ;
068800150723       //--------------------------------------------------------------
068900150723       //?controlli SFL
069000150723       //--------------------------------------------------------------
069100150723       BEGSR ContrS01;
069200150723
069300150723       s01nrr=1  ;
069400150723       readc lsi1s01    ;
069500150723
069600150723 0     dow not %eof                      ;
069700150723
069800150723       // Verifica l'opzione
069900150723          if vs1opz<>*blanks    ;
070000150723            *in19=*off;
070100150723          select;
070200150723          when vs1opz='1'                     ;
070300150724            chain (vshaas:vshlnp:vshnrs:vshnsp) fnblp01l ;
070400150914                 kaas=blpaas;
070500150914                 klnp=blplnp;
070600150914                 knrs=blpnrs;
070700150914                 knsp=blpnsp;
070800150728            exsr ctr_sfl;
070900150724            if *in28;
071000150724               eval *in19=*on;
071100150724            else;
071200150724               exsr calpgm;
071300150724            endif;
071400150724            chain (vshaas:vshlnp:vshnrs:vshnsp) fnblp01l ;
071500150723            if blpdcm>0;
071600150723               eval *in19=*on;
071700150723            endif;
071800150723          when vs1opz='5';
071900150723               exsr int_bolla;
072000150723          endsl;
072100150723               clear vs1opz;
072200150723               update  lsi1s01  ;
072300150723          endif;
072400150723
072500150723       readc    lsi1s01    ;
072600150723 0     enddo         ;
072700150723
072800150723       ENDSR  ;
072900150723       //--------------------------------------------------------------
073000150723       //?Aggiornamento sfl
073100150723       //--------------------------------------------------------------
073200150723       BEGSR AGGIOSFL  ;
073300150723
073400150723       //Sflnxtchg=*on  ;
073500150723
073600150723       update  lsi1s01  ;
073700150723
073800150723       ENDSR   ;
073900150723      /end-free
074000150723     C*--------------------------------------------------------------*
074100150723     C* interroga bolla in partenza
074200150723     C*--------------------------------------------------------------*
074300150723     C     int_bolla     BEGSR
074400150723     C                   CLEAR                   PARA05
074500150723     C                   Z-ADD     VshAAS        P05AAS
074600150723     C                   Z-ADD     VshLNP        P05LNP
074700150723     C                   Z-ADD     VshNRS        P05NRS
074800150723     C                   Z-ADD     VshNSP        P05NSP
074900150723     C                   MOVEL     '1'           P05GIA
075000150723     C                   MOVEL     PARA05        KPJBU
075100150723     C                   CALL      'FNLS05R'
075200150723     C                   PARM                    KPJBA
075300150723     c                   endsr
075400970410     C*--------------------------------------------------------------*
075500130822     C* Decodifica i 2 riferimenti per impostare la spedizione
075600970410     C*--------------------------------------------------------------*
075700130822     C     decod_da_Rif  BEGSR
075800970410     C*
075900131016     c                   clear                   piu_bolle_Xrif    1
076000131016     c                   clear                   V2NOTE1
076100131016     c                   clear                   V2NOTE2
076200131016     C*
076300130822     c                   clear                   se_trovato        1
076400130822     c                   eval      se_trovato ='N'
076500130822     C*
076600130822     c                   if        v2rif32 <> *blank
076700131016     c     v2rif32       setll     fnblp32l
076800131016     c     v2rif32       reade     fnblp32l
076900131016     c                   dow       not %EoF(fnblp32l)
077000130822     c                   eval      se_trovato ='S'
077100131016     c     v2rif32       reade     fnblp32l
077200131016     c                   if        not %EoF(fnblp32l)
077300131016     c                   eval       piu_bolle_Xrif ='S'
077400131016     c                   end
077500130822     c                   end
077600130822     c                   end
077700130822      *
077800130822     c                   if        v2rif33 > 0 and
077900130822     c                             se_trovato ='N'
078000131016     c     v2rif33       setll     fnblp33l
078100131016     c     v2rif33       reade     fnblp33l
078200131016     c                   dow       not %Eof(fnblp33l)
078300130822     c                   eval      se_trovato ='S'
078400131016     c     v2rif33       reade     fnblp33l
078500131016     c                   if        not %EoF(fnblp33l)
078600131016     c                   eval       piu_bolle_Xrif ='S'
078700131016     c                   end
078800130822     c                   end
078900130822     c                   end
079000130822     C*
079100130822     C* Se lo ha trovato imposta imposta i campi della spedizione
079200131016     C*   ATTENZIONE potrebbero essercene più di una con lo stesso
079300131016     C* riferimento quindi segnala che ha preso l'ultima
079400130822     c                   IF        se_trovato ='S'
079500131016     c                               and v2cnsp = 0
079600130822     c                   eval      v2caas = blpaas
079700130822     c                   eval      v2clnp = blplnp
079800130822     c                   eval      v2cnrs = blpnrs
079900130822     c                   eval      v2cnsp = blpnsp
080000130822     c                   end
080100131016     C*   AVVISA SE HA trovato più bolle con lo stesso riferimento
080200131016     c                   IF         piu_bolle_Xrif ='S'
080300131016     c                   EVAL        V2NOTE1 = err(25)
080400131016     c                   EVAL        V2NOTE2 = err(26)
080500131016     c                   end
080600131016     C*
080700130822     C                   ENDSR
080800130822     C*--------------------------------------------------------------*
080900130822     C* CTR02: Controlli prima videata
081000130822     C*--------------------------------------------------------------*
081100130822     C     CTR02         BEGSR
081200130822     C*
081300130822     C*  pre- controllo bolla tramite rif.alfa o numerico
081400130822     c                   exsr      decod_da_Rif
081500130822     C*
081600970410     C* Controllo se immesso anno
081700970611     C     V2CAAS        IFEQ      0
081800970410     C                   SETON                                        2840
081900970611     C                   MOVEL     ERR(5)        $MSG
082000970410     C                   END
082100990610     C   28              GOTO      FINCT2
082200970410     C* Se anno digitato di 2 --> 4 cifre
082300970612     C                   MOVEL     V2CAAS        WSECOL            2 0
082400970410     C     WSECOL        IFEQ      0
082500970611     C     V2CAAS        IFGT      60
082600970611     C                   ADD       1900          V2CAAS
082700970410     C                   ELSE
082800970611     C                   ADD       2000          V2CAAS
082900970410     C                   END
083000970410     C                   END
083100970410     C* Controllo se immessa linea di partenza
083200970611     C     V2CLNP        IFEQ      0
083300970414     C                   SETON                                        2841
083400970611     C                   MOVEL     ERR(6)        $MSG
083500970410     C                   END
083600990610     C   28              GOTO      FINCT2
083700970410     C* Controllo esistenza lnp
083800970611     C                   Z-ADD     V2CLNP        ORGFIL
083900970410     C     ORGFIL        CHAIN     AZORG01L                           31
084000970611     C     *IN31         IFEQ      '1'
084100970410     C                   SETON                                        2841
084200970611     C                   MOVEL     ERR(7)        $MSG
084300970414     C                   END
084400990610     C   28              GOTO      FINCT2
084500970414     C* Controllo se lnp in £1
084600970611     C     V2CLNP        LOOKUP    £1                                     32
084700970414     C     *IN32         IFEQ      '0'
084800970414     C                   SETON                                        2841
084900970611     C                   MOVEL     ERR(8)        $MSG
085000970414     C                   END
085100990610     C   28              GOTO      FINCT2
085200970414     C* Controllo se immesso numero di spedizione
085300970611     C     V2CNSP        IFEQ      0
085400970414     C                   SETON                                        2843
085500970611     C                   MOVEL     ERR(9)        $MSG
085600970414     C                   END
085700990610     C   28              GOTO      FINCT2
085800970414     C* Controllo se esiste spedizione
085900970611     C                   Z-ADD     V2CAAS        KAAS
086000970611     C                   Z-ADD     V2CLNP        KLNP
086100970611     C                   Z-ADD     V2CNRS        KNRS
086200970611     C                   Z-ADD     V2CNSP        KNSP
086300970414     C     KBLP          CHAIN(N)  FNBLP01L                           31
086400970414     C     *IN31         IFEQ      '1'
086500970414     C                   SETON                                        2843
086600970611     C                   MOVEL     ERR(10)       $MSG
086700970616     C                   ELSE
086800970616     C* Controllo che la spedizione sia del cliente selezionato
086900970616     C     BLPCCM        IFNE      V2CKSC
087000970616     C     BLPKSC        ANDNE     V2CKSC
087100970616     C                   SETON                                        2843
087200980202     C                   MOVEL     ERR(17)       $MSG
087300970414     C                   END
087400970616     C                   END
087500990610     C   28              GOTO      FINCT2
087600970414     C* Controllo se bolla mamma
087700150728     C*                  Z-ADD     V2CAAS        KAAP
087800150728     C*                  Z-ADD     V2CLNP        KLNP
087900150728     C*                  Z-ADD     V2CNRS        KNRP
088000150728     C*                  Z-ADD     V2CNSP        KNSP
088100150728     C*    KBLP          CHAIN     FNLBL02L                           31
088200970414     C* Controllo se bolla figlia
088300150728     C*                  Z-ADD     V2CAAS        KAAP
088400150728     C*                  Z-ADD     V2CLNP        KLNP
088500150728     C*                  Z-ADD     V2CNRS        KNRP
088600150728     C*                  Z-ADD     V2CNSP        KNSP
088700150728     C*    KBLP          CHAIN     FNLBL01L                           31
088800150728     C*    *IN31         IFEQ      '0'
088900150728     C*                  SETON                                        284344
089000150728     C*                  MOVEL     ERR(12)       $MSG
089100150728     C*                  END
089200150728     C*  28              GOTO      FINCT2
089300980204     C* Controllo che non sia stata data per consegnata
089400150728     C*    BLPDCM        IFNE      0
089500150728     C*    BLPCCA        ANDNE     '6'
089600150728     C*    BLPDCM        ORNE      0
089700150728     C*    BLPCCA        ANDNE     '2'
089800150728     C*                  SETON                                        2843
089900150728     C*                  MOVEL     ERR(22)       $MSG
090000150728     C*                  END
090100150728     C*  28              GOTO      FINCT2
090200980204     C* Controllo che abbia il dettaglio segnacolli
090300150728     C*                  Z-ADD     V2CAAS        KAAS
090400150728     C*+                 Z-ADD     V2CLNP        KLNP
090500150728     C**                 Z-ADD     V2CNRS        KNRS
090600150728     C*                  Z-ADD     V2CNSP        KNSP
090700150728     C*    KBLT          CHAIN     FNBLT01L                           31
090800150728     C*    *IN31         IFEQ      '1'
090900150728     C*                  SETON                                        2843
091000150728     C*                  MOVEL     ERR(16)       $MSG
091100150728     C*                  END
091200150728     c                   exsr      ctr_uniS
091300150728     C   28              GOTO      FINCT2
091400980202     C* Controllo se richiesto '?'
091500980202     C     '?'           SCAN      V2CSTS                                 32
091600980202     C     *IN32         IFEQ      '1'
091700980202     C                   MOVE      LAB(XY)       DI1LAB
091800980202     C                   MOVEL     FNLSI1        KPJBU
091900980203     C                   CALL      'FNLSI1R5'
092000980202     C                   PARM                    KPJBA
092100980204     C                   MOVEL     KPJBU         FNLSI1
092200980204     C                   MOVEL     DI1CEV        V2CCOD
092300980202     C                   MOVEL     DI1STS        V2CSTS
092400980202     C                   END
092500980202     C* evento obbligatoria
092600980202     C     V2CCOD        IFEQ      *BLANKS
092700980202     C     V2CSTS        ANDEQ     *BLANKS
092800970721     C                   SETON                                        2844
092900980202     C                   MOVEL     ERR(21)       $MSG
093000970721     C                   END
093100990610     C   28              GOTO      FINCT2
093200980202     C* evento esistente
093300980202     C                   MOVEL     V2CSTS        WEVE              6
093400980202     C                   MOVE      V2CCOD        WEVE
093500030116     C                   Z-ADD     1             YY                5 0
093600980202     C     LAB(XY)       IFEQ      'S'
093700980202     C     WEVE          LOOKUP    ELB(YY)                                32
093800980202     C                   MOVEL     DLB(YY)       DSEM
093900980202     C                   ELSE
094000980202     C     WEVE          LOOKUP    EVG(YY)                                32
094100980202     C                   MOVEL     DVG(YY)       DSEM
094200980203     C                   MOVEL     §EMDES        V2DCOD
094300980202     C                   END
094400980204     C     *IN32         IFEQ      '0'
094500980202     C                   SETON                                        2844
094600980202     C                   MOVEL     ERR(19)       $MSG
094700980202     C                   END
094800990610     C   28              GOTO      FINCT2
094900980203     C* Se bolla in assegnato no IDD Totale definitivo
095000150728     C*                  Z-ADD     1             X
095100150728     C*    BLPCBO        LOOKUP    C3A(X)                                 32
095200150728     C*                  MOVEL     TPO(X)        WTPO              1
095300150728     C*    *IN32         IFEQ      '1'
095400150728     C*    WTPO          ANDEQ     'A'
095500150728     C*    §EMASS        ANDNE     'S'
095600150728     C*                  SETON                                        2843
095700150728     C*                  MOVEL     ERR(20)       $MSG
095800150728     C*                  END
095900150728     C*  28              GOTO      FINCT2
096000970414     C* Controllo che non sia stata data per partita
096100150728     C*    BLPDT1        IFNE      0
096200150728     C*    §EMTRA        ANDNE     'S'
096300150728     C*                  SETON                                        2843
096400150728     C*                  MOVEL     ERR(13)       $MSG
096500150728     C*                  END
096600150728     C*  28              GOTO      FINCT2
096700970414     C* Controllo che non sia stata trasmessa in sede
096800150728     C*    BLPDT2        IFNE      0
096900150728     C*    §EMTRA        ANDNE     'S'
097000150728     C*                  SETON                                        2843
097100150728     C*                  MOVEL     ERR(14)       $MSG
097200150728     C*                  END
097300150728     C*  28              GOTO      FINCT2
097400970414     C* Controllo che non sia stata trasmessa al cliente
097500150728     C*    BLPDT3        IFNE      0
097600150728     C*    §EMTRA        ANDNE     'S'
097700150728     C*                  SETON                                        2843
097800150728     C*                  MOVEL     ERR(15)       $MSG
097900150728     C*                  END
098000150728     C*  28              GOTO      FINCT2
098100990610      * Controllo C.A.
098200150728     C*    §EMCAC        IFEQ      'S'
098300040715      *
098400150728     c*                  clear                   fidn12ds
098500150728     c*                  eval      i12aas = V2CAAS
098600150728     c*                  eval      i12lnp = V2CLNP
098700150728     c*                  eval      i12nrs = V2CNRS
098800150728     c*                  eval      i12nsp = V2CNSP
098900150728     c*                  eval      i12fel = 'E'
099000150728     c*                  eval      i12fan = 'E'
099100150728     c*                  eval      i12fch = 'E'
099200150728     c*                  call      'FIDN12R'
099300150728     c*                  parm                    fidn12ds
099400040715      **
099500040715      * se c'è almeno una C.A. aperta
099600150728     c*    o12nca        ifgt      0
099700150728     C*                  SETON                                        284331
099800150728     C*                  MOVEL     ERR(24)       $MSG
099900150728     c*                  end
100000040715      **
100100150728     C*                  ENDIF
100200040715      **
100300150728     c                   exsr      ctr_uniE
100400990610     C   28              GOTO      FINCT2
100500040715      **
100600980204     C* Controllo se l'evento è già stato inviato
100700980204     C                   Z-ADD     V2CAAS        KAA2
100800980204     C                   MOVEL     §EMCOD        KCEV
100900980204     C                   MOVEL     ' '           WDAFOR            1
101000990810     C     KEVB          CHAIN     FNEVB04L                           31
101100980204     C* Evento già immesso
101200980204     C     *IN31         IFEQ      '0'
101300980204     C* se non è possibile forzare do errore
101400980204     C     §EMFOR        IFNE      'S'
101500980204     C                   SETON                                        2843
101600980204     C                   MOVEL     ERR(23)       $MSG
101700980204     C                   ELSE
101800980204     C                   MOVEL     'F'           WDAFOR
101900980204     C                   END
102000980204     C                   END
102100990610     C   28              GOTO      FINCT2
102200970414     C*
102300970612     C     FINCT2        ENDSR
102400150724     C*--------------------------------------------------------------*
102500150728     C*    Controlli per il caricamento e gestione sfl
102600150724     C*--------------------------------------------------------------*
102700150728     C     ctr_sfl       BEGSR
102800150724     c                   clear                   $msg
102900150724     c                   setoff                                       28
103000150728     c                   exsr      ctr_uniS
103100150728     c                   if        *in28
103200150728     c                   leavesr
103300150728     c                   endif
103400150728     c                   exsr      ctr_uniE
103500150728     c                   if        *in28
103600150728     c                   leavesr
103700150728     c                   endif
103800150724     c                   clear                   wsispu            1
103900150724     c* controllo che la spedizione non abbia spunte
104000150728     c     kblt          setll     fnblt01l
104100150728     c     kblt          reade     fnblt01l                               99
104200150724    2c                   dow       not *in99
104300150724     c
104400150724     c* Cerco una qualsiasi spunta
104500150724     c     kbrv          chain     fnbrv07l
104600150724    3c                   if        %found(fnbrv07l)
104700150724     c                   eval      wsispu='S'
104800150724     c                   seton                                        99
104900150724   x3c                   else
105000150728     c     kblt          reade     fnblt01l                               99
105100150724    3c                   endif
105200150724    2c                   enddo
105300150724     c*
105400150724    2c                   if        wsispu='S'
105500150724     c                   seton                                        28
105600150724     C                   MOVEL     ERR(29)       $MSG
105700150724     c                   leavesr
105800150724     c                   endif
105900150724     c
106000150724     c
106100150724     c                   endsr
106200150728     C*--------------------------------------------------------------*
106300150728     C*    Controlli unici per spedizione
106400150728     C*--------------------------------------------------------------*
106500150728     C     ctr_uniS      BEGSR
106600150728     C* Controllo se bolla figlia
106700150728     C     KBLP          CHAIN     FNLBL01L
106800150728     C                   if        %found(fnlbl01l)
106900150728     C                   SETON                                        28
107000150728     C                   SETON                                          4344
107100150728     C                   MOVEL     ERR(12)       $MSG
107200150728     c                   leavesr
107300150728     C                   END
107400150728     C* Controllo che non sia stata data per consegnata
107500150728     C     BLPDCM        IFNE      0
107600150728     C     BLPCCA        ANDNE     '6'
107700150728     C     BLPDCM        ORNE      0
107800150728     C     BLPCCA        ANDNE     '2'
107900150728     C                   SETON                                        28
108000150728     C                   SETON                                        43
108100150728     C                   MOVEL     ERR(22)       $MSG
108200150728     c                   leavesr
108300150728     C                   END
108400150728     C* Controllo che abbia il dettaglio segnacolli
108500150728     c     kblt          setll     fnblt01l
108600150728     C                   IF        not %equal(fnblt01l)
108700150728     C                   SETON                                        28
108800150728     C                   SETON                                        43
108900150728     C                   MOVEL     ERR(16)       $MSG
109000150728     c                   leavesr
109100150728     C                   END
109200150728     c                   endsr
109300150728     C*--------------------------------------------------------------*
109400150728     C*    Controlli unici legati all'evento
109500150728     C*--------------------------------------------------------------*
109600150728     C     ctr_uniE      BEGSR
109700150728     C* Se bolla in assegnato no IDD
109800150728     C                   Z-ADD     1             X
109900150728     C     BLPCBO        LOOKUP    C3A(X)                                 32
110000150728     C                   MOVEL     TPO(X)        WTPO              1
110100150728     C     *IN32         IFEQ      '1'
110200150728     C     WTPO          ANDEQ     'A'
110300150728     C     §EMASS        ANDNE     'S'
110400150728     C                   SETON                                        28
110500150728     C                   SETON                                        43
110600150728     C                   MOVEL     ERR(20)       $MSG
110700150728     c                   leavesr
110800150728     C                   END
110900150728     C* Controllo che non sia stata data per partita
111000150728     C     BLPDT1        IFNE      0
111100150728     C     §EMTRA        ANDNE     'S'
111200150728     C                   SETON                                        2843
111300150728     C                   MOVEL     ERR(13)       $MSG
111400150728     c                   leavesr
111500150728     C                   END
111600150728     C* Controllo che non sia stata trasmessa in sede
111700150728     C     BLPDT2        IFNE      0
111800150728     C     §EMTRA        ANDNE     'S'
111900150728     C                   SETON                                        2843
112000150728     C                   MOVEL     ERR(14)       $MSG
112100150728     c                   leavesr
112200150728     C                   END
112300150728     C* Controllo che non sia stata trasmessa al cliente
112400150728     C     BLPDT3        IFNE      0
112500150728     C     §EMTRA        ANDNE     'S'
112600150728     C                   SETON                                        2843
112700150728     C                   MOVEL     ERR(15)       $MSG
112800150728     c                   leavesr
112900150728     C                   END
113000150728      * Controllo C.A.
113100150728      *
113200150728     C     §EMCAC        IFEQ      'S'
113300150728     c                   clear                   fidn12ds
113400150728     c                   eval      i12aas = blpaAS
113500150728     c                   eval      i12lnp = blpLNP
113600150728     c                   eval      i12nrs = blpNRS
113700150728     c                   eval      i12nsp = blpNSP
113800150728     c                   eval      i12fel = 'E'
113900150728     c                   eval      i12fan = 'E'
114000150728     c                   eval      i12fch = 'E'
114100150728     c                   call      'FIDN12R'
114200150728     c                   parm                    fidn12ds
114300150728      **
114400150728      * se c'è almeno una C.A. aperta
114500150728     c     o12nca        ifgt      0
114600150728     C                   SETON                                        28
114700150728     C                   SETON                                          4331
114800150728     C                   MOVEL     ERR(24)       $MSG
114900150728     c                   leavesr
115000150728     c                   end
115100150728     C                   ENDIF
115200150728     c                   endsr
115300970612     C*--------------------------------------------------------------*
115400970612     C* CALPGM: Eseguo Call a pgm
115500970612     C*--------------------------------------------------------------*
115600970612     C     CALPGM        BEGSR
115700970612     C*
115800970612     C*  Imposto DS per richiamo altri pgm
115900970612     C                   CLEAR                   FNLSI1
116000150723     c                   if        wtpvid='S'
116100150723     C                   clear                   DI1FTX
116200150723     C                   MOVEL     V1CKSC        DI1KSC
116300150723     C                   MOVEL     VhdKSC        DI1DEC
116400150723     C                   MOVEL     VshLNP        DI1LNP
116500150723     C                   MOVEL     VshNRS        DI1NRS
116600150723     C                   MOVEL     VshNSP        DI1NSP
116700150723     C                   MOVEL     VshAAS        DI1AAS
116800150723     C                   MOVEL     VshAAS        DI1DSP
116900150723     C                   MOVE      BLPMGS        DI1DSP
117000150723     C                   MOVE      LAB(XY)       DI1LAB
117100150723     C                   MOVE      wsts          DI1STS
117200150723     C                   MOVE      wcev          DI1CEV
117300160209     C**                 MOVEL     wsts          WEVE              6
117400160209     C**                 MOVE      wcev          WEVE
117500150723
117600160209     C***                Z-ADD     1             YY                5 0
117700160209     C**   LAB(XY)       IFEQ      'S'
117800160209     C***  WEVE          LOOKUP    ELB(YY)                                32
117900160209     C***                MOVEL     DLB(YY)       DSEM
118000160209     C***                ELSE
118100160209     C***  WEVE          LOOKUP    EVG(YY)                                32
118200160209     C***                MOVEL     DVG(YY)       DSEM
118300160209     C***                END
118400150723
118500150723     C                   MOVE      §EMDES        DI1DEV
118600150723     C                   MOVE      §EMDES        DI1DEV
118700150723     C                   MOVE      §EMCOD        DI1C2A
118800150723     C                   MOVE      §EMCCA        DI1CCA
118900150723     C                   MOVE      §EMBLP        DI1BLP
119000150723     C                   MOVE      §EMBLT        DI1BLT
119100150723     C                   MOVE      §EMFOR        DI1FRZ
119200150723     C                   MOVE      §EMTOT        DI1TOT
119300150723     C                   MOVE      BLPNCL        DI1NCL
119400150723     c                   else
119500980203     C                   MOVEL     V2CFTX        DI1FTX
119600980203     C                   MOVEL     V2CKSC        DI1KSC
119700970612     C                   MOVEL     V2DKSC        DI1DEC
119800970612     C                   MOVEL     V2CLNP        DI1LNP
119900970612     C                   MOVEL     V2CNRS        DI1NRS
120000970612     C                   MOVEL     V2CNSP        DI1NSP
120100970612     C                   MOVEL     V2CAAS        DI1AAS
120200970612     C                   MOVEL     V2CAAS        DI1DSP
120300970612     C                   MOVE      BLPMGS        DI1DSP
120400970616     C                   MOVE      BLPMGS        DI1DSP
120500971007     C                   MOVE      LAB(XY)       DI1LAB
120600150723     C                   MOVE      V2CSTS        DI1STS
120700150723     C                   MOVE      V2CCOD        DI1CEV
120800980202     C                   MOVE      §EMDES        DI1DEV
120900980203     C                   MOVE      §EMDES        DI1DEV
121000980203     C                   MOVE      §EMCOD        DI1C2A
121100980203     C                   MOVE      §EMCCA        DI1CCA
121200980203     C                   MOVE      §EMBLP        DI1BLP
121300980203     C                   MOVE      §EMBLT        DI1BLT
121400980203     C                   MOVE      §EMFOR        DI1FRZ
121500980203     C                   MOVE      §EMTOT        DI1TOT
121600980204     C                   MOVE      WDAFOR        DI1FOR
121700980204     C                   MOVE      BLPNCL        DI1NCL
121800150723     c                   endif
121900970612     C                   MOVEL     FNLSI1        KPJBU
122000980202     C* Richiamo pgm gestione dettaglio segnacolli
122100980202     C     §EMTOT        IFNE      'S'
122200980202     C                   CALL      'FNLSI1R2'
122300980202     C                   PARM                    KPJBA
122400980202     C                   MOVEL     KPJBU         FNLSI1
122500980202     C                   END
122600980202     C* Se ho digitato F3 esco
122700980202     C     DI1CMD        IFEQ      '03'
122800980202     C                   MOVEL     'S'           WFINE
122900980202     C                   GOTO      FINRIC
123000980202     C                   END
123100980204     C* Se ho digitato F12 azzero campo scelta
123200980204     C     DI1CMD        IFEQ      '12'
123300980204     C                   GOTO      FINRIC
123400980204     C                   END
123500980202     C* Richiamo pgm gestione terza videata: SPEDIZIONE
123600970612     C                   CALL      'FNLSI1R1'
123700970612     C                   PARM                    KPJBA
123800970617     C                   MOVEL     KPJBU         FNLSI1
123900970612     C*
124000970612     C* Se ho digitato F3 esco
124100970612     C                   SELECT
124200970612     C     DI1CMD        WHENEQ    '03'
124300970619     C                   MOVEL     'S'           WFINE
124400970619     C* Controllo se devo richiamare TRTC81
124500970619     C     WRIC81        IFEQ      'S'
124600970619     C                   EXSR      CALL81
124700970619     C                   END
124800970612     C* Se ho digitato F12 azzero campo scelta
124900970612     C     DI1CMD        WHENEQ    '12'
125000970616     C* CONFERMA
125100970616     C     DI1CMD        WHENEQ    '06'
125200970617     C                   MOVEL     'S'           WRIC81
125300970616     C* Aggiorno BLP/BLT
125400980202     C     §EMBLP        IFEQ      'S'
125500970612     C                   CALL      'FNLSI1R3'
125600970617     C                   PARM                    KPJBA
125700970616     C                   END
125800080421      * l'EDIVAC viene sostituito con il TIVGD Download standard
125900080421     C* Scrivo EDVAC --> TIVGD00F
126000970612     C                   CALL      'FNLSI1R4'
126100970612     C                   PARM                    KPJBA
126200970616     C                   ENDSL
126300970612     C*
126400980202     C     FINRIC        ENDSR
126500970617     C*--------------------------------------------------------------*
126600980126     C* CALL81: PER E.D.I. RICHIAMO PGM TRTC81C
126700970617     C*--------------------------------------------------------------*
126800970617     C     CALL81        BEGSR
126900970617     C*
127000980126      * CALL A TRTC81C PER E.D.I.
127100980126     C     §PTFTT        IFNE      'S'
127200061124     C     §PTFTT        ANDNE     'P'
127300980126     C                   MOVEL     'S'           WCAL81            1
127400080421      *
127500080421      *  Scrive IFTSTA x EDI:
127600080421      * ? il TRTC81C  attiva giro di trasm con SNDIFTSTA  ?
127700080421     C                   MOVEL     *all'0'       Elab_Cliente
127800080421     C                   MOVE      CKS(XX)       Elab_Cliente
127900080421     c                   eval      Tipo_Scarico = CSD(XX)
128000080421      *
128100080421     c                   eval      Tipo_File = 'VC'
128200110630     C                   CALL      'TRTCT81C'
128300080421     c                   parm                    Elab_Cliente      8
128400080422     c                   parm                    Tipo_File         2
128500080421     c                   parm                    Tipo_Scarico      2
128600980126      *
128700980126     C                   END
128800980126      *
128900970617     C                   ENDSR
129000970410     C*--------------------------------------------------------------*
129100970410     C* *INZSR: Operazioni iniziali
129200970410     C*--------------------------------------------------------------*
129300970410     C     *INZSR        BEGSR
129400970410     C*
129500970410     C     *ENTRY        PLIST
129600970410     C                   PARM                    KPJBA
129700990223     C* Richiamo xparut
129800970410     C                   Z-ADD     1             CODUT
129900970410     C                   CALL      'X§PARUT'
130000970410     C                   PARM                    UTEDSE
130100970410     C                   MOVEL     REC80         CNCR80
130200970410     C                   MOVEL     RAGUT         RSUT
130300030924      *
130400030924      *  In base all'utente prende il P.O.
130500030924     C                   CLEAR                   Tibs36Ds
130600030924     c                   EVAL      I36ute = Knmus
130700030924     c                   EVAL      I36Tla = 'L'
130800030924     C                   CALL      'TIBS36R'
130900030924     C                   PARM                    Tibs36Ds
131000030924      *
131100970612     C*--- RICERCA CAPOCONTI------------------------------------------*
131200030116     C                   DO        50            X                 5 0
131300970612     C                   MOVE      TCU(X)        TCUDS
131400970612     C     F56           IFEQ      'CG'
131500970612     C     F4            ANDEQ     '1'
131600970612     C     F3            ANDEQ     '0'
131700970612     C                   Z-ADD     KCU(X)        CLIITA            4 0
131800970612     C                   END
131900970612     C                   END
132000970612     C*--- RICERCA CAPOCONTI------------------------------------------*
132100970410      *  Definizione chiavi
132200970410     C     KBLP          KLIST
132300970410     C                   KFLD                    KAAS
132400970410     C                   KFLD                    KLNP
132500970410     C                   KFLD                    KNRS
132600970410     C                   KFLD                    KNSP
132700970410     C     KBLT          KLIST
132800970410     C                   KFLD                    KAAS
132900970410     C                   KFLD                    KLNP
133000970410     C                   KFLD                    KNRS
133100970410     C                   KFLD                    KNSP
133200970410     C     KLBL1         KLIST
133300970410     C                   KFLD                    KAAP
133400970410     C                   KFLD                    KLPP
133500970410     C                   KFLD                    KNRP
133600970410     C                   KFLD                    KNSP
133700970410     C     KLBL2         KLIST
133800970410     C                   KFLD                    KAAN
133900970410     C                   KFLD                    KLPN
134000970410     C                   KFLD                    KNRN
134100970410     C                   KFLD                    KNSN
134200970410     C     KTAB1         KLIST
134300970410     C                   KFLD                    KKUT
134400970410     C                   KFLD                    KCOD
134500970410     C     KTAB2         KLIST
134600970410     C                   KFLD                    KKUT
134700970410     C                   KFLD                    KCOD
134800970410     C                   KFLD                    KKEY
134900970612     C     KACO          KLIST
135000970612     C                   KFLD                    KKUT
135100970612     C                   KFLD                    KKCC
135200970612     C                   KFLD                    KKSC
135300980204     C     KEVB          KLIST
135400980204     C                   KFLD                    KAA2
135500980204     C                   KFLD                    KLNP
135600980204     C                   KFLD                    KNRS
135700980204     C                   KFLD                    KNSP
135800980204     C                   KFLD                    KCEV
135900080918     C     KTBE1         KLIST
136000080918     C                   KFLD                    TBECOD
136100080918     C                   KFLD                    TBEKE1
136200150415     C     Kedtab        KLIST
136300150415     C                   KFLD                    tabCOD
136400150415     C                   KFLD                    tabKEY
136500150724     C     KBrv          KLIST
136600150724     C                   KFLD                    bltfls
136700150724     C                   KFLD                    bltlna
136800150724     C                   KFLD                    bltNRS
136900150724     C                   KFLD                    bltnsc
137000970410      *  Definizione variabili
137100980204     C     *LIKE         DEFINE    EVBAAS        KAA2
137200980204     C     *LIKE         DEFINE    EVBCEV        KCEV
137300970410     C     *LIKE         DEFINE    BLPAAS        KAAS
137400970410     C     *LIKE         DEFINE    BLPLNP        KLNP
137500970410     C     *LIKE         DEFINE    BLPNRS        KNRS
137600970410     C     *LIKE         DEFINE    BLPNSP        KNSP
137700970410     C     *LIKE         DEFINE    LBLAAP        KAAP
137800970410     C     *LIKE         DEFINE    LBLLPP        KLPP
137900970410     C     *LIKE         DEFINE    LBLNRP        KNRP
138000970410     C     *LIKE         DEFINE    LBLAAN        KAAN
138100970410     C     *LIKE         DEFINE    LBLLPN        KLPN
138200970410     C     *LIKE         DEFINE    LBLNRN        KNRN
138300970410     C     *LIKE         DEFINE    LBLNSN        KNSN
138400970410     C     *LIKE         DEFINE    TBLKUT        KKUT
138500970410     C     *LIKE         DEFINE    TBLCOD        KCOD
138600970410     C     *LIKE         DEFINE    TBLKEY        KKEY
138700970611     C     *LIKE         DEFINE    TABCOD        KCODE
138800970612     C     *LIKE         DEFINE    ACOKCC        KKCC
138900970612     C     *LIKE         DEFINE    ACOKSC        KKSC
139000970410      *
139100970410      *  Reperisco la data del giorno
139200970410     C                   TIME                    WHHDAT           14 0
139300970414     C                   MOVE      WHHDAT        G02DAT
139400970414     C                   MOVEL     *BLANK        G02ERR
139500970414     C                   CALL      'XSRDA8'
139600970414     C                   PARM                    WLBDA8
139700970414     C                   Z-ADD     G02INV        OGGI              8 0
139800970611     C*
139900970611     C* Carico dati DS partner in schiera
140000970612     C                   OPEN      EDTAB01L
140100150415     C                   Z-ADD     0             X
140200970611     C                   MOVEL     'PT'          KCODE
140300970616     C     KCODE         CHAIN     EDTAB01L                           30
140400970611     C     *IN30         DOWEQ     '0'
140500970611     C     TABFLG        IFEQ      ' '
140600970611     C                   ADD       1             X
140700970611     C                   MOVEL     TABUNI        DSPT
140800150415     C                   MOVEL     TABKEY        KEYPT(X)
140900970611     C                   MOVEL     §PTKSC        KSC(X)
141000970616     C                   MOVEL     §PTLNP        LNP(X)
141100970616     C                   MOVEL     §PTNRS        NRS(X)
141200970611     C                   END
141300970616     C     KCODE         READE     EDTAB01L                               30
141400970611     C                   END
141500970616     C*
141600150415     C* x evitare che a causa di un disallineamento imprevisto
141700150415     C*  la PU possa essere disallineata alla PT  eseguo un caricamento
141800150415     C*  differente.
141900150415     c                   clear                   lab
142000150415     c                   do        x             pux               5 0
142100150415     C                   MOVEL     'PU'          tabCOD
142200150415     C                   MOVEL     KEYPT(pux)    tabKEY
142300150415     C     Kedtab        CHAIN     EDTAB01L
142400150415     c                   if        %Found(edtab01l) and tabflg =' '
142500150415      *  Lista Labarta
142600150415     C                   MOVEL     TABUNI        DSPU
142700150415     C                   MOVEL     §PULAB        LAB(pux)
142800150415     c                   end
142900150415     c                   enddo
143000150415      *
143100150415      **** vecchio caricamento
143200150415     C****               Z-ADD     0             X
143300150415     C****               MOVEL     'PU'          KCODE
143400150415     C**** KCODE         CHAIN     EDTAB01L                           30
143500150415     C**** *IN30         DOWEQ     '0'
143600150415     C**** TABFLG        IFEQ      ' '
143700150415     C****               ADD       1             X
143800150415     C****               MOVEL     TABUNI        DSPU
143900150415     C****               MOVEL     §PULAB        LAB(X)
144000150415     C****               END
144100150415     C**** KCODE         READE     EDTAB01L                               30
144200150415     C****               END
144300150415      *
144400150415     C*  Carico in schiera codici collegati - CL -
144500150415     C                   Z-ADD     0             X
144600150415     C                   MOVEL     'CL'          KCODE
144700150415     C     KCODE         CHAIN     EDTAB01L                           30
144800150415     C     *IN30         DOWEQ     '0'
144900150415     C     TABFLG        IFEQ      ' '
145000150415     C                   ADD       1             X
145100150415     C                   MOVEL     TABUNI        DSCL
145200150415     C                   MOVEL     §CLKSC        clKSC(X)
145300150415     C                   MOVEL     §CLLNP        clLNP(X)
145400150415     C                   END
145500150415     C     KCODE         READE     EDTAB01L                               30
145600150415     C                   END
145700980202     C*
145800980202     C* Carico in schiera tabella eventi manuali
145900150723     c                   clear                   wcev
146000150723     c                   clear                   wsts
146100980202     C                   Z-ADD     0             X
146200030116     C                   Z-ADD     0             X1                5 0
146300980202     C                   MOVEL     'EM'          KCODE
146400980202     C     KCODE         CHAIN     EDTAB01L                           30
146500980202     C     *IN30         DOWEQ     '0'
146600980202     C     TABFLG        IFEQ      ' '
146700980202     C                   MOVEL     TABUNI        DSEM
146800980202     C     §EMLAB        IFEQ      'S'
146900980202     C                   ADD       1             X
147000980204     C                   MOVEL     TABKEY        ELB(X)
147100980202     C                   MOVEL     TABUNI        DLB(X)
147200980202     C                   END
147300980202     C     §EMVGL        IFEQ      'S'
147400980202     C                   ADD       1             X1
147500980204     C                   MOVEL     TABKEY        EVG(X1)
147600980202     C                   MOVEL     TABUNI        DVG(X1)
147700980202     C                   END
147800150723     c* memorizzo il codice dell'evento corrispondente alla chiusura bolla
147900150723     c* per merce mai affidata
148000150723     c                   if        §emmai='S'
148100150723     c                   movel     tabkey        wsts
148200150723     c                   eval      wcev=%subst(tabkey:4:3)
148300160209     c                   eval      s_dsem=dsem
148400150723     c                   endif
148500980202     C     KCODE         READE     EDTAB01L                               30
148600980202     C                   END
148700980202     C                   END
148800970611     C* close EDTAB
148900970611     C                   CLOSE     EDTAB01L
149000970611     C*
149100970410     C* Eseguo caricamento tabella 3K
149200970612     C                   OPEN      TABEL00F
149300970410     C                   Z-ADD     0             X
149400970410     C                   Z-ADD     1             KKUT
149500970410     C                   MOVEL     '3K'          KCOD
149600970410     C     KTAB1         CHAIN     TABEL                              30
149700970410     C     *IN30         DOWEQ     '0'
149800030213     C     X             ANDLT     9990
149900970410     C     TBLFLG        IFEQ      *BLANKS
150000970410     C                   ADD       1             X
150100970617     C                   MOVEL     TBLUNI        DS3K
150200970617     C                   MOVEL     TBLKEY        C3K(X)
150300970617     C                   MOVEL     §3KCKS        CKS(X)
150400010725     C                   MOVEL     §3KCSD        CSD(X)
150500970410     C                   END
150600970410     C     KTAB1         READE     TABEL                                  30
150700970410     C                   END
150800970715     C* Eseguo caricamento tabella 3A
150900970721     C                   Z-ADD     0             X
151000970715     C                   MOVEL     '3A'          KCOD
151100970715     C     KTAB1         CHAIN     TABEL                              30
151200970715     C     *IN30         DOWEQ     '0'
151300970721     C     X             ANDLT     500
151400970715     C     TBLFLG        IFEQ      *BLANKS
151500970715     C                   ADD       1             X
151600970715     C                   MOVEL     TBLUNI        DS3A
151700970715     C                   MOVEL     TBLKEY        C3A(X)
151800970715     C                   MOVEL     §3ATB1        TPO(X)
151900970715     C                   END
152000970715     C     KTAB1         READE     TABEL                                  30
152100970715     C                   END
152200970701     C*
152300970701     C* Eseguo caricamento da tabella 7J codici pistole fittizie
152400970701     C                   MOVEA     *HIVAL        C7J
152500970701     C                   Z-ADD     0             X
152600970701     C                   Z-ADD     1             KKUT
152700970701     C                   MOVEL     '7J'          KCOD
152800970701     C     KTAB1         CHAIN     TABEL                              30
152900970701     C     *IN30         DOWEQ     '0'
153000970721     C     X             ANDLT     500
153100970701     C     TBLFLG        IFEQ      *BLANKS
153200970701     C                   MOVEL     TBLUNI        DS7J
153300970912     C     §7JRPS        IFEQ      'A'
153400970701     C                   ADD       1             X
153500970701     C                   MOVEL     TBLKEY        C7J(X)
153600970701     C                   END
153700970701     C                   END
153800970701     C     KTAB1         READE     TABEL                                  30
153900970701     C                   END
154000150728
154100150728     C                   MOVEL     '3I'          Kcod
154200150728     C                   MOVEL     '1       '    kKEY
154300150728     c                   clear                   ds3i
154400150728     C     KTAB2         CHAIN     TABEL00F
154500150728     c                   if        %found(tabel00f)
154600150728     C                   MOVEL     TBLUNI        DS3I
154700150728     c                   endif
154800970410     C* Eseguo caricamento £1
154900020506     c                   clear                   trul06ds
155000020506     c                   eval      d06cod = '£1'
155100030924     c                   movel     o36pou        d06key
155200020506     c                   movel     trul06ds      kpjbu
155300020506     c                   call      'TRUL06R'
155400020506     c                   parm                    kpjba
155500020506     c                   movel     kpjbu         trul06ds
155600020506     c                   movea     lin           £1
155700150724     c* terminal di partenza del p.o. utente
155800150724     c                   clear                   fnlv55ds
155900150724     C                   MOVEL     'P'           D55TPT
156000150724     C                   MOVEL     simpou        D55LIN
156100150724     C                   z-add     oggi          D55drf
156200150724     C                   CALL      'FNLV55R'
156300150724     C                   PARM                    fnlv55ds
156400150724     c                   z-add     d55tfp        wtfp
156500020912      *
156600971008     C* Imposto a 'N' flag richiamo TRTC81C
156700971008     C                   MOVEL     'N'           WCAL81            1
156800970410     C* Eseguo close del file
156900970410     C                   CLOSE     TABEL00F
157000970410     C*
157100970410     C                   ENDSR
157200080930      *------------------------------------------------------------------------*
157300080930      * Routine di controllo x eventuali Errori
157400080930      *------------------------------------------------------------------------*
157500080930     C     *PSSR         BEGSR
157600080930      *
157700080930      * Comunque eseguo x rilasciare il TIVGD.
157800080930      *? Infine elimino il blocco elaborazione TIVGD x tipo file qui.  ?
157900080930     C                   clear                   trul47ds
158000080930     C                   eval      d47opz  = 'F'
158100080930     C                   eval      d47tip  = 'VC'
158200080930     C                   call      'TRUL47R'
158300080930     C                   parm                    trul47ds
158400080930      *
158500080930     C                   ENDSR     '*CANCL'
158600080930     C*------------------------------------------------------------------------*
158700971008**
158800970701Immettere codice cliente                                                      01
158900970701Codice cliente inesistente                                                    02
159000060811Codice cliente non presente in tabella 3K o PT                                03
159100970701Un altro utente sta chiudendo le spedizioni del cliente                       04
159200970701Immettere anno della spedizione                                               05
159300970701Immettere linea di partenza della spedizione                                  06
159400970701Linea di partenza inesistente                                                 07
159500970701Linea di partenza non gestita su questo as                                    08
159600970701Immettere numero della spedizione                                             09
159700970701Spedizione inesistente                                                        10
159800980206Bolla mamma: impossibile gestirla                                             11
159900980206Bolla figlia: impossibile gestirla                                            12
160000970701Spedizione già trasmessa alla filiale di arrivo                               13
160100970701Spedizione già trasmessa alla sede                                            14
160200970701Spedizione già trasmessa al cliente                                           15
160300980202Spedizione senza colli: impossibile eseguire chiusura                         16
160400980202Spedizione non appartenente al cliente selezionato                            17
160500980202Il codice selezionato deve appartenere ad un partner                          18
160600980202Codice evento inesistente                                                     19
160700980202Bolla in assegnato: chiusura definitiva possibile solo in arrivo              20
160800980202Codice evento obbligatorio                                                    21
160900980204Spedizione già consegnata                                                     22
161000980204Evento già inviato impossibile rinviarlo                                      23
161100990610Esistono C.A. aperte per la spedizione, codice evento non valido              24
161200131016 ATTENZIONE: più bolle con stesso riferimento!!                               25
161300131016 VERIFICARE se la Spedizione è quella corretta.                               26
161400150723Nessun dato da visualizzare per la selezione richiesta                        27
161500150728                                                                              28
161600150724Spedizione non gestibile:presenti spunte                                      29
161700150728Attenzione: caricate solo 9.999 bolle ma ne sono presenti ancora              30
161800150909Non trovato Evento per Chiusura per Merce mai Affidata. Contattare il CED (EM)31
