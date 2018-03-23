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
007300160407     D ERR             S             78    DIM(32) CTDATA PERRCD(1)
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
008800970410     D*--------------------------------------------------------------*
008900970410     D* DS
009000970410     D*--------------------------------------------------------------*
009100030924     D Tibs36ds      E DS
009200970612     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
009300970612     D  TCU                  398    697
009400970612     D                                     DIM(50)
009500970612     D  KCU                  698    847P 0
009600970612     D                                     DIM(50)
009700970612     D                                     PACKEVEN
009800970612     D TCUDS           DS
009900970612     D  F3                     3      3
010000970612     D  F4                     4      4
010100970612     D  F56                    5      6
010200970612     D CNCR80        E DS
010300020506     d trul06ds      e ds
010400020506     d  lin                    1     90  0 dim(30)
010500970612     D* DS  appoggio x cmd
010600970616     D CMDAPP          DS            50
010700970616     D  NOMMBR                42     48
010800970617     D CMDLOG          DS            80
010900970617     D  MBRLOG                42     48
011000970617     D  MBRFIS                71     77
011100150724     d fnlv55ds      e ds
011200970414     D WLBDA8          DS
011300970414     D  G02DAT                 1      8  0
011400970414     D  G02INV                 9     16  0
011500970414     D  G02ERR                17     17
011600970414     D  G02TGI                18     22  0
011700150728     d WDAT8           DS
011800150728     d  datada                 1      8  0
011900150728     d  dataa                  9     16  0
012000150728     d  ggl                   17     21  0
012100080421      *----------------------------------------------------------------
012200080421      * Controllo e semaforo x scrittura del VAC: TRUL47R
012300080421      *----------------------------------------------------------------
012400080421     D trul47ds      e ds
012500160317
012600160317     d fior51ds      e ds
012700080421      *
012800970410     D KPJBA         E DS
012900150723     d fnblpds       e ds                  extname(fnblp00f) inz
013000970612     D DSPT          E DS                  EXTNAME(EDIDSPT)
013100970616     D DSPU          E DS                  EXTNAME(EDIDSPU)
013200980202     D DSEM          E DS                  EXTNAME(EDIDSEM)
013300160209     D s_dsem        E DS                  EXTNAME(EDIDSEM)
013400160209     D                                     prefix(S_)
013500150415     D DSCL          E DS                  EXTNAME(EDIDSCL)
013600970616     D* DATI X RICHIAMO PGM INTERR. BOLLE PARTENZE (FNLS05R)
013700970616     D PARA04          DS                  INZ
013800970616     D  P04AAS                 1      4  0
013900970616     D  P04LNP                 5      7  0
014000970616     D  P04NRS                 8      9  0
014100970616     D  P04NSP                10     16  0
014200980204     D  P04F03                17     17
014300970616     D  P04FLG                31     31
014400970616     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
014500970616     D  P04GIA                48     48
014600970616      * PA3CLI indica se il pgm e' richiamato da un cliente (='S')
014700970616     D  P04CLI               143    143
014800970616     D PARA05          DS                  INZ
014900970616     D  P05AAS                 1      4  0
015000970616     D  P05LNP                 5      7  0
015100970616     D  P05NRS                 8      9  0
015200970616     D  P05NSP                10     16  0
015300970616     D  P05F03                17     17
015400970616     D  P05FLG                18     18
015500970616     D  P05SIM                44     46  0
015600970616     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
015700970616     D  P05GIA                48     48
015800080421     D***** TIBS55        E DS                  EXTNAME(TIBS55DS)
015900971126     D FNLSI1        E DS                  EXTNAME(FNLSI1DS)
016000970410     D DS3K          E DS
016100970715     D DS3A          E DS
016200970701     D DS7J          E DS
016300970410     D DS7O          E DS
016400150728     D DS3i          E DS
016500060811     D OG143         E DS
016600160308     D dsbl4m        E DS
016700040715
016800040715     d Fidn12ds      e ds
016900040715     d  skKey                 26    305    dim(20)
017000040715     d  skAnn                306    325    dim(20)
017100040715     d  skDca                326    485    dim(20)
017200040715     d  skFca                486    545  0 dim(20)
017300040715     d  skDch                546    705  0 dim(20)
017400040715     d  skCch                706    745    dim(20)
017500040715
017600970410     D                SDS
017700970410     D  NOMPGM                 1     10
017800970410     I*--------------------------------------------------------------*
017900970410     I*  Definizione campi Azorg
018000970410     I*--------------------------------------------------------------*
018100150724     I***AZORG01L  AA
018200150724     I***                                  1    3  ORGDIT
018300150724     I***                             P    4    5 0ORGFIL
018400150724     I***                               3776 3800  ORGDE3
018500160317
018600160317     d fior51r         pr                  extpgm('FIOR51R')
018700160317     d  fior51ds                           likeds(fior51ds)
018800160317
018900150728      /copy gaitrasrc/srcprotopr,xsrlav8
019000970410     C*--------------------------------------------------------------*
019100970410     C*  C I C L O     P R I N C I P A L E
019200970410     C*--------------------------------------------------------------*
019300150723      /free
019400150723         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
019500150723      /end-free
019600970410     C*  Inizializzo varibili videata
019700970410     C                   EXSR      INZ01
019800080421      *
019900970410     C*  Gestione della prima videata
020000970410     C     WFINE         DOWEQ     'N'
020100970611     C     WTPVID        CASEQ     '1'           GESD01
020200970611     C     WTPVID        CASEQ     '2'           GESD02
020300150723     C     WTPVID        CASEQ     'S'           GESsfl
020400970410     C                   END
020500970611     C                   END
020600970410     C*  Fine pgm
020700970410     C                   SETON                                        LR
020800970611     C*--------------------------------------------------------------*
020900970611     C* INZ01: Impostazione della prima videata
021000970611     C*--------------------------------------------------------------*
021100970611     C     INZ01         BEGSR
021200970611     C*
021300970611     C                   SETOFF                                       2840
021400970616     C                   MOVEL     '1'           WTPVID
021500970617     C                   MOVEL     'N'           WRIC81            1
021600970616     C                   MOVEL     'N'           WFINE
021700970611     C                   MOVEL     NOMPGM        V1CPGM
021800970611     C                   Z-ADD     0             V1CKSC
021900160408     c                   movel     'E'           v1cass
022000970611     C*
022100970611     C                   ENDSR
022200970410     C*--------------------------------------------------------------*
022300970611     C* INZ02: Impostazione della prima videata
022400970410     C*--------------------------------------------------------------*
022500970611     C     INZ02         BEGSR
022600970410     C*
022700970410     C                   SETOFF                                       284041
022800970611     C                   SETOFF                                       424344
022900970616     C                   MOVEL     '2'           WTPVID
023000970611     C                   MOVEL     NOMPGM        V2CPGM
023100970611     C                   MOVEL     V1CKSC        V2CKSC
023200970611     C                   MOVEL     ACORAG        V2DKSC
023300980204     C                   MOVEL     *BLANKS       V2DCOD
023400980204     C                   MOVEL     *BLANKS       V2CCOD
023500980202     C                   MOVEL     *BLANKS       V2CSTS
023600980202     C                   MOVEL     *BLANKS       V2CFTX
023700970611     C                   MOVE      WHHDAT        V2CAAS
023800130304      **  gestione cliente con Partner gestito da altra filiale
023900130304      **  vedi GWEISS con clienti sulla 277.
024000130304     c                   if        LNP_ALTRA_FIL = 0
024100971007     C                   Z-ADD     LNP(XY)       V2CLNP
024200130821      ******
024300130821     c**** Chiodo x Ducros da Eliminare fra 333 e 330
024400130821     c                   if        v2clnp =333
024500130821     c                   eval      v2clnp =330
024600130821     c                   end
024700130821      ******
024800130304     c                   else
024900130304     C                   eval        V2CLNP = LNP_ALTRA_FIL
025000130304     c                   end
025100971007     C                   Z-ADD     NRS(XY)       V2CNRS
025200970611     C                   Z-ADD     0             V2CNSP
025300970410     C*
025400970410     C                   ENDSR
025500970410     C*--------------------------------------------------------------*
025600970410     C* GESD01: Gestione della prima videata
025700970410     C*--------------------------------------------------------------*
025800970410     C     GESD01        BEGSR
025900970410     C*
026000970410     C* Eseguo exfmt della prima videata
026100970410     C                   EXFMT     LSI1D01
026200970611     C                   SETOFF                                       2840
026300970410     C                   SETOFF                                       4243
026400160407     C                   SETOFF                                       46
026500970410     C* Fine
026600970410     C     *INKC         IFEQ      '1'
026700970414     C                   MOVEL     'S'           WFINE             1
026800970410     C                   GOTO      FINVD1
026900970410     C                   END
027000970410     C* Controlli
027100970410     C                   EXSR      CTR01
027200970410     C   28              GOTO      FINVD1
027300970611     C*
027400970611     C* Se no errori passo a videata successiva
027500150723     c* chiusura bolle per merce mai affidata --> la videata successiva è il sfl
027600150723     c* delle bolle
027700150723     c                   if        v1cfbo='S'
027800150723     C                   MOVEL     'S'           WTPVID
027900150723     c                   eval      $inzS01=*on
028000150723     c                   else
028100150724     C                   EXSR      INZ02
028200150723     C                   MOVEL     '2'           WTPVID
028300150723     c                   endif
028400970410     C*
028500970410     C     FINVD1        ENDSR
028600970611     C*--------------------------------------------------------------*
028700970611     C* GESD02: Gestione della seconda videata
028800970611     C*--------------------------------------------------------------*
028900970611     C     GESD02        BEGSR
029000970611     C*
029100970611     C* Eseguo exfmt della prima videata
029200970611     C                   EXFMT     LSI1D02
029300970611     C                   SETOFF                                       284041
029400970611     C                   SETOFF                                       424344
029500970611     C* Fine
029600970611     C     *INKC         IFEQ      '1'
029700970611     C                   MOVEL     'S'           WFINE             1
029800970617     C* Controllo se devo richiamare TRTC81
029900970617     C     WRIC81        IFEQ      'S'
030000970617     C                   EXSR      CALL81
030100970617     C                   END
030200970611     C                   GOTO      FINVD2
030300970611     C                   END
030400970611     C* Prima videata
030500970611     C     *INKL         IFEQ      '1'
030600970611     C                   MOVEL     '1'           WTPVID            1
030700970617     C* Controllo se devo richiamare TRTC81
030800970617     C     WRIC81        IFEQ      'S'
030900970617     C                   EXSR      CALL81
031000970617     C                   END
031100970611     C                   GOTO      FINVD2
031200970611     C                   END
031300080421      *
031400970612     C* Interrogazione bolle
031500970612     C     *INKG         IFEQ      '1'
031600970616     C     V2CNSP        IFGT      0
031700970616     C                   CLEAR                   PARA05
031800970616     C                   Z-ADD     V2CAAS        P05AAS
031900970616     C                   Z-ADD     V2CLNP        P05LNP
032000970616     C                   Z-ADD     V2CNRS        P05NRS
032100970616     C                   Z-ADD     V2CNSP        P05NSP
032200970616     C                   MOVEL     '1'           P05GIA
032300970616     C                   MOVEL     PARA05        KPJBU
032400970616     C                   CALL      'FNLS05R'
032500970616     C                   PARM                    KPJBA
032600970616     C                   MOVEL     KPJBU         PARA05
032700980204     C     P05F03        IFEQ      *BLANKS
032800980204     C                   Z-ADD     P05AAS        V2CAAS
032900980204     C                   Z-ADD     P05LNP        V2CLNP
033000980204     C                   Z-ADD     P05NRS        V2CNRS
033100980204     C                   Z-ADD     P05NSP        V2CNSP
033200980204     C                   ENDIF
033300970616     C*
033400970616     C                   ELSE
033500970616     C*
033600970616     C                   CLEAR                   PARA04
033700970616     C                   MOVEL     '1'           P04FLG
033800970616     C                   MOVEL     PARA04        KPJBU
033900970616     C                   CALL      'FNLS04R'
034000970616     C                   PARM                    KPJBA
034100980204     C                   MOVEL     KPJBU         PARA04
034200980204     C     P04F03        IFEQ      *BLANKS
034300980204     C                   Z-ADD     P04AAS        V2CAAS
034400980204     C                   Z-ADD     P04LNP        V2CLNP
034500980204     C                   Z-ADD     P04NRS        V2CNRS
034600980204     C                   Z-ADD     P04NSP        V2CNSP
034700970616     C                   ENDIF
034800980204     C                   ENDIF
034900970612     C                   GOTO      FINVD2
035000970612     C                   END
035100970611     C* Controlli
035200970611     C                   EXSR      CTR02
035300970611     C   28              GOTO      FINVD2
035400970611     C*
035500970611     C* Se no errori vado a pgm successivo
035600970611     C                   EXSR      CALPGM
035700970616     C* Azzero prima videata se ho eseguito conferma
035800970616     C     DI1CMD        IFEQ      '06'
035900970617     C                   EXSR      INZ02
036000970616     C                   END
036100970611     C*
036200970611     C     FINVD2        ENDSR
036300970611     C*--------------------------------------------------------------*
036400970611     C* CTR01: Controlli prima videata
036500970611     C*--------------------------------------------------------------*
036600970611     C     CTR01         BEGSR
036700970611     C*
036800970611     C* Controllo se immesso codice cliente
036900970611     C     V1CKSC        IFEQ      0
037000970611     C                   SETON                                        2840
037100970611     C                   MOVEL     ERR(1)        $MSG
037200970611     C                   END
037300990610     C   28              GOTO      FINCT1
037400150415      *
037500970611     C* Controllo se esiste cliente
037600970612     C                   Z-ADD     1             KKUT
037700970611     C                   Z-ADD     CLIITA        KKCC
037800970611     C                   Z-ADD     V1CKSC        KKSC
037900970611     C     KACO          CHAIN     CNACO00F                           31
038000970611     C     *IN31         IFEQ      '1'
038100150723     c                   clear                   vhdksc
038200970611     C                   SETON                                        2840
038300970611     C                   MOVEL     ERR(2)        $MSG
038400970611     C                   END
038500990610     C   28              GOTO      FINCT1
038600150723     c                   movel     acorag        vhdksc
038700150415      ****
038800970617     C* Controllo Codice cliente unificante tabella 3K
038900030116     C                   Z-ADD     1             XX                5 0
039000970617     C     V1CKSC        LOOKUP    C3K(XX)                                32
039100060811      *
039200060811      **?se non ha agganciato nulla
039300060811     c                   if        *in32 = *off
039400150415      *
039500060811     c     v1cksc        div       10000         trelnp            3 0
039600060811     C                   Z-ADD     treLNP        ORGFIL
039700060811     C     ORGFIL        CHAIN     AZORG01L                           31
039800060811      **? e se  si tratta di DPD (eccezione .......)
039900060811     c                   if        %Found(AZORG01L)
040000060811     c                   movel     orgde3        og143
040100060811     c                   if        §OGNTW ='DPD'
040200060811     C                   Z-ADD     1             XY                5 0
040300150415      ** va direttamente sulla PT poichè DPD non sta sulla 3K
040400060811     C     V1CKSC        LOOKUP    KSC(XY)                                32
040500060811     c                   end
040600060811     c                   end
040700060811      **
040800060811     c                   else
040900060811      **
041000150415      **  con l'unificante della 3K
041100970617     C* Controllo se cliente in tabella PT
041200060811     C                   Z-ADD     1             XY                5 0
041300060811     C     CKS(XX)       LOOKUP    KSC(XY)                                32
041400150415      **
041500060811     c                   end
041600060811      **
041700150415      **  Se non l'avesse trovato con l'UNIFICANTE sulla PT
041800970611     C     *IN32         IFEQ      '0'
041900150415      **
042000970611     C                   SETON                                        2840
042100970611     C                   MOVEL     ERR(3)        $MSG
042200150415      **
042300970617     C                   ELSE
042400020912      *
042500020912      *  la Linea di Partenza del Partner deve essere gestita
042600020912      *   nella £1
042700020912     C     LNP(XY)       lookup    £1                                     32
042800080918      ** ----
042900130304      **     Potrebbe essere un cliente appartenente alla tabella CL
043000130304      **     gestito da un'altra filiale. a questo punto occorre controllare,
043100130304      **     non solo la LINEA del PARTNER agganciata sull'unificante della 3K
043200130304      **     ma se non fosse gestito il Partner sulla filiale si deve vedere
043300130304      **     la Linea del Cliente se è gestita sulla filiale.
043400130304      **   (Esempio su clienti di GWeiss della 360 dati in gestione alla 277.
043500130304      **    il cliente è il 2772159 unificato sulla 3K al 3600001.
043600130304      **   Non è gestita la 360 nella 277 allora il cliente 2772159 è della 277
043700130304      **    quindi deve passare il controllo...poichè il cliente è in gestione)
043800130304     c                   clear                   LNP_ALTRA_FIL     3 0
043900150415      **
044000130304     c                   if        NOT *in32
044100150415      **
044200150415      ** Dovendo gestire casi in cui il cliente è stato spostato ANCORA di
044300150415      **  Filiale mantenendo il codice cliente, occorre effettuare il
044400150415      ** ragionamento sulla LNP presente sulla tab.CL o sulla PT per
044500150415      ** verificare l'appartenenza alla £1 e NON con i primi 3 caratteri del
044600150415      ** codice cliente.
044700150415      ** E'sempre il caso della GWEISS che adesso da 277 viene spostato alla
044800150415      **  249 mantenendo il codice cliente come 277xxxx e facendo capo alla 360
044900150415      **
045000150415     c                   clear                   treLNP            3 0
045100150415     c**** v1cksc        div       10000         treLNP            3 0
045200150415      ** ---
045300150415      **  cerca sulla CL  x impostare la LNP
045400150415     C                   Z-ADD     1             clpt              5 0
045500150415     C     V1CKSC        LOOKUP    clKSC(clpt)                            32
045600150415     c                   if        *in32
045700150415     c                   z-add     clLNP(clpt)   treLNP
045800150415     c                   else
045900150415      **  se non la trova cerca sulla PT  x impostare la LNP
046000150415     C                   Z-ADD     1             clpt              5 0
046100150415     C     V1CKSC        LOOKUP    KSC(clpt)                              32
046200150415     c   32              z-add     LNP(clpt)     treLNP
046300150415     c                   end
046400150415      ** ---
046500150415      **
046600130304     C     treLNP        lookup    £1                                     32
046700130304     c   32              movel     treLNP        LNP_ALTRA_FIL
046800150415      **
046900130304     c                   end
047000130304      ** ----
047100080918      ** Occorre anche controllare se DPD di backup quindi linee collegate
047200080918      **  per potere gestire linee da filiali diverse
047300080918      **   ossia tab."3NN"
047400080918      **
047500080918     C     *IN32         ifeq      *off
047600080918     c                   movel(p)  '3NN'         TbeCOD
047700080918     c                   movel(p)  KSC(xy)       TbeKE1
047800080918     c     kTBE1         setll     tntbe01l
047900080918     c     kTBE1         reade     tntbe01l
048000080918     C                   dow       not %EoF(tntbe01l)
048100080918     c                   movel(p)  tbeKE2        LNPAlternativa    3 0
048200080918     C     LNPAlternativalookup    £1                                     32
048300080918     c                   if        *in32
048400080918     c                   z-add     1             z                 3 0
048500080918     c     *zero         lookup    £1(z)                                  21
048600080918      * aggiunge la linea x il controllo successivo
048700080918     c                   if        *in21
048800080918     C                   z-add     lnp(xy)       £1(z)
048900080918     C                   End
049000080918     C                   Leave
049100080918     C                   End
049200080918     c     kTBE1         reade     tntbe01l
049300080918     C                   End
049400080918     C                   End
049500080918      ** ----
049600020912     C     *IN32         ifeq      *off
049700020912     C                   SETON                                        2840
049800020912     C                   MOVEL     err(8)        $MSG
049900020912     C                   Else
050000020912     C*
050100020912     C                   EndIF
050200020912     C*
050300970617     C                   END
050400020912     C*
050500990610     C   28              GOTO      FINCT1
050600150909     c* Se richiesta visualizzazione bolle da chiudere per merce mai
050700150909     c* affidata ERRORE se non trovato l'evento su tabella EM
050800150909     c                   if        v1cfbo='S' and wsts=*blanks
050900150909     C                   SETON                                        28
051000150909     C                   MOVEL     err(31)       $MSG
051100150909     c                   endif
051200160407     c* Se richiesta visualizzazione bolle da chiudere per merce mai
051300160407     c* il porto deve essere "A" o "F"
051400160407     c                   if        v1cfbo='S' and v1cass=*blanks
051500160407     C                   SETON                                        2846
051600160407     C                   MOVEL     err(32)       $MSG
051700160407     c                   endif
051800080421     C*
051900970611     C     FINCT1        ENDSR
052000150723     C*--------------------------------------------------------------*
052100150723     C* gessfl:  videata sfl per codice cliente
052200150723     C*--------------------------------------------------------------*
052300150723     C     gessfl        BEGSR
052400150723      /free
052500160209         // Valorizzo DSEM con i dati dell'evento di chisura per merce mai aff.
052600160209         dsem=s_dsem ;
052700160209
052800150723         if $inzs01=*on;
052900150723            exsr inzs01;
053000150723            c01csr=1;
053100150723            c01rcd=1;
053200150723            $inzs01=*off;
053300150723         // Se non trovati record errore in prima videata
053400150723            if s01nrr=0   ;
053500150723               $msg=err(27)  ;
053600150723               *in28=*on  ;
053700150723               *in40=*on  ;
053800150723               wtpvid='1'  ;
053900150723               leavesr   ;
054000150723            endif  ;
054100160408       // Le spedizioni in assegnato (orm commissionati) non verranno chiuse
054200160408       // da qui e quindi anche se caricata una sola spedizione non vado mai
054300160408       // alla videata singola ma passo sempre dal sfl
054400160408
054500160408       //   if s01nrr=1;
054600160408       //     chain (vshaas:vshlnp:vshnrs:vshnsp) fnblp01l ;
054700160408       //        kaas=blpaas;
054800160408       //        klnp=blplnp;
054900160408       //        knrs=blpnrs;
055000160408       //        knsp=blpnsp;
055100160408       //     ctrvid=*on ;
055200160408       //     exsr ctr_sfl   ;
055300160408       //     if not *in28  ;
055400160408       //        exsr calpgm;
055500160408       //        chain (vshaas:vshlnp:vshnrs:vshnsp) fnblp01l ;
055600160408       //        if %found(fnblp01l);
055700160408       //           exsr chiuORM;
055800160408       //        endif;
055900160408       //        wtpvid='1' ;
056000160408       //        leavesr;
056100160408       //     endif;
056200160408       //   endif;
056300150723         endif;
056400150723       // c01csr   contiene il numero di riga del subfile su cui era posizionato
056500150723       // impostando c01rcd   visualizzo la pagina che vedeva l'utente quando ha
056600150723       // l'ultimo tasto
0567001507232         if c01csr   > 0;
056800150723           c01rcd   = c01csr;
056900150723          else;
057000150723           c01rcd   = s01rcd   ;
0571001507232         endif;
057200150723       // se non so quale pagina visualizzare forzo pagina 1
0573001507232         if c01rcd   < 1;
057400150723           eval c01rcd   = 1;
0575001507232         endif;
057600150723
057700150723       // valorizzo campi del sfl control
057800150723          vc1ksc=%int(v1cksc);
057900150724          vc1kscd=vhdksc;
058000160408          if v1cass='I' ;
058100160408             vc1por='ORM Commissionati';
058200160407          else;
058300160408             clear vc1por              ;
058400160407          endif;
058500150723
058600150723         // Emissione videata
058700150723         write  lsi1z01;
058800150723         exfmt  lsi1C01;
058900150723
059000150723         s01nrr=1  ;
059100150723
059200150723 1       SELECT;
059300150723
059400150723         // - F3=Fine
059500150723 1         WHEN  *inkc;
059600150723               wFine = 'S';
059700150723
059800150723         // - F12=Ritorno
059900150723 1         WHEN  *inkl;
060000150914               *in28=*off;
060100150723               wtpvid='1'  ;
060200150723
060300150723
060400150723         // -
060500150723 1       //WHEN  *inkb ;
060600150723         //  exsr seltot;
060700150723
060800150723
060900150723 x1      // Invio
061000150723           OTHER;
061100150723
061200150723               exsr ContrS01 ;
061300150723
061400150723 1       ENDSL;
061500150723      /end-free
061600150723     c                   endsr
061700150723      /free
061800150723       //--------------------------------------------------------------
061900150723       BEGSR InzS01;
062000150723
062100150723       // Pulizia subfile
062200150723         *in30  = *on;
062300150723         *in31  = *on;
062400150723         write  lsi1c01;
062500150723         *in31  = *off;
062600150723         *in30  = *off;
062700150723         *in32  = *off;
062800150723
062900150723         exec sql close BLPcsr;
063000150723       // Richiamo routine di preparazione stringa SQL
063100150723             exsr sr_prepsql;
063200150723
063300150723         S01nrr=0 ;
063400150723         clear $msg;
063500150723         *in28  = *off;
063600150723         *in90  = *off;
063700150728         *in19=*off;
063800160308         *in45=*off;
063900150723       // Elaboro i records estratti
064000150723           $finerec=*off;
064100150723           exec sql prepare STRINGASQL from :WrkStringaSql;
064200150723           exec sql declare BLPCSR cursor for StringaSql;
064300150723           exec sql open BLPcsr;
064400150723           dow $finerec=*off;
064500150723              exec sql Fetch BLPcsr into :fnblpds;
064600150728              *in28  = *off;
064700150728              if sqlcod=100 or sqlcod<0;
064800150723                 $finerec = *on;
064900150723                 leave;
065000150723              endif;
065100150728              if s01nrr=9999;
065200150728                 $finerec = *on;
065300150728                 $msg=err(30);
065400150728                 *in28=*on;
065500150728                 leave;
065600150728              endif;
065700150723              if %lookup(blplnp:£1)>0;
065800150724                 kaas=blpaas;
065900150724                 klnp=blplnp;
066000150724                 knrs=blpnrs;
066100150724                 knsp=blpnsp;
066200160405       // Vedo se assegnato o franco
066300160407                 kkey=blpcbo;
066400160407                 clear ds3a;
066500160407                 chain (1:'3A':kkey) tabel00f;
066600160407                 if %found(tabel00f);
066700160407                    ds3a=tbluni;
066800160407                 endif;
066900160407       // escludo in base al porto richiesto
067000160408                 if (v1cass='I' and %subst(§3atb1:1:1)='A')  or
067100160408                    (v1cass='E' and %subst(§3atb1:1:1)<>'A')  ;
067200150728       //per poter chiudere la spedizione come merce mai affidata
067300150728       //devono essere trascorsi i giorni max di sospensione previsti
067400150728       //da tabella
067500160405       //I giorni variano a seconda che si tratti di assegnato oppure franco
067600160405
067700150728                 clear wdat8;
067800150728                 datada=blpdim;
067900150728                 dataa=(blpaas*10000)+blpmgs;
068000150728                 xsrlav8(wdat8);
068100160407                 if (%subst(§3atb1:1:1)<>'A' and (ggl-1) >  §3iggc) or
068200160407                    (%subst(§3atb1:1:1)='A' and (ggl-1) >  §3igga) ;
068300160307                    ctrvid=*off;
068400150728                    exsr ctr_sfl;
068500150728                    if not *in28;
068600150728           // Carico record nel sfl
068700150728                       exsr carsf01;
068800150728                    endif;
068900150728                 endif;
069000160405               endif;
069100150723              endif;
069200150723           enddo;
069300150723
069400150723          s01rcd    = s01nrr  ;
069500150723
069600150723         ENDSR  ;
069700150723       //--------------------------------------------------------------
069800150723       //?Preparazione stringa SQL
069900150723       //--------------------------------------------------------------
070000150723       BEGSR sr_prepsql;
070100150723       WrkStringaSql='select * from fnblp00f' +
070200150724       ' where (blpksc=' + %editc(v1cksc:'X') + ' or '      +
070300150724       ' blpccm=' + %editc(v1cksc:'X') +
070400150724       ') and blptfp=' + %editc(wtfp:'X') +
070500150723       ' and blpnfv=0' +
070600150723       ' order by  blptfp, blpnfv, blpdim';
070700150723       endsr;
070800150723       //--------------------------------------------------------------
070900150723       //?eseguo caricamento SFL
071000150723       //--------------------------------------------------------------
071100150723          BEGSR   carSF01    ;
071200150723          clear vs1opz  ;
071300150723
071400150723          exsr CarRECORD  ;
071500150723          vshaas=blpaas;
071600150723          vshlnp=blplnp;
071700150723          vshnrs=blpNRs;
071800150723          vshnsp=blpNsp;
071900150723          s01nrr=s01nrr+1   ;
072000150723          write  lsi1S01;
072100150723
072200150723          ENDSR         ;
072300150723       //--------------------------------------------------------------
072400150723       //?Caricamento record SFL
072500150723       //--------------------------------------------------------------
072600150723       BEGSR CarRecord;
072700150723       // data Immissione bolla
072800150723             clear vs1dim;
072900150723             datadmy=%date(blpdim:*iso);
073000150723             vs1dim=%dec(datadmy);
073100150723       // Data Spedizione
073200150723             clear vs1dsp;
073300150723             eval datadmy=%date((blpaas*10000+blpmgs):*iso);
073400150723             vs1dsp=%dec(datadmy);
073500150723       // linea di arrivo
073600150723             vs1lna=blplna;
073700150723             chain blplna azorg01l ;
073800150723             if orgde5<>*blanks;
073900150723                 vs1lnad=%subst(orgde5:1:10);
074000150723             else;
074100150723                 vs1lnad=%subst(orgdes:1:10);
074200150723             endif;
074300150723       // Destinatario
074400150723             vs1rsd=blprsd;
074500160317       // Visualizzo se assegnato
074600160407             clear vs1ass;
074700160408               if v1cass='I';
074800160408                  vs1ass=§3atb1;
074900160408               endif;
075000150723       ENDSR         ;
075100150723       //--------------------------------------------------------------
075200150723       //?controlli SFL
075300150723       //--------------------------------------------------------------
075400150723       BEGSR ContrS01;
075500150723
075600160308       *in32=*off ;
075700150723       s01nrr=1  ;
075800150723       readc lsi1s01    ;
075900150723
076000150723 0     dow not %eof                      ;
076100150723
076200160308            *in45=*off;
076300160308            *in28=*off;
076400150723       // Verifica l'opzione
076500150723          if vs1opz<>*blanks    ;
076600150723            *in19=*off;
076700150723          select;
076800150723          when vs1opz='1'                     ;
076900150724            chain (vshaas:vshlnp:vshnrs:vshnsp) fnblp01l ;
077000150914                 kaas=blpaas;
077100150914                 klnp=blplnp;
077200150914                 knrs=blpnrs;
077300150914                 knsp=blpnsp;
077400160308            ctrvid=*on ;
077500160308            exsr ctr_sfl;
077600150724            if *in28;
077700160308       //      eval *in19=*on;
077800160308               eval *in45=*on;
077900160308               eval *in32=*on;
078000150724            else;
078100160308               exsr calpgm;
078200150724            endif;
078300150724            chain (vshaas:vshlnp:vshnrs:vshnsp) fnblp01l ;
078400160317       // Se spedizione in assegnato richiamo fior51r per eventuale chiusura
078500160317       // dell'ORM commissionato
078600160317            if %found(fnblp01l);
078700160330       //      if vs1ass<>*blanks;
078800160330       //         chain (blpaas:blplnp:blpnrs:blpnsp:'M') fiar401l;
078900160330       //         if %found(fiar401l);
079000160330       //           dsbl4m=ar4not;
079100160330       //           clear fior51ds;
079200160330       //           I51POE=§B4POE;
079300160330       //           I51NSR=§B4NSR;
079400160330       //           I51NOR=§B4NOR;
079500160330       //           I51NRV=§B4NRV;
079600160330       //           I51FAR=999;
079700160330       //           fior51r(fior51ds);
079800160330       //         endif;
079900160330       //      endif;
080000160330               exsr chiuORM;
080100150723            if blpdcm>0;
080200150723               eval *in19=*on;
080300150723            endif;
080400160317            endif;
080500150723          when vs1opz='5';
080600150723               exsr int_bolla;
080700150723          endsl;
080800150723               clear vs1opz;
080900150723          endif;
081000160308               update  lsi1s01  ;
081100160308
081200160308          *in32=*off ;
081300150723
081400150723       readc    lsi1s01    ;
081500150723 0     enddo         ;
081600150723
081700150723       ENDSR  ;
081800150723       //--------------------------------------------------------------
081900150723       //?Aggiornamento sfl
082000150723       //--------------------------------------------------------------
082100150723       BEGSR AGGIOSFL  ;
082200150723
082300150723       //Sflnxtchg=*on  ;
082400150723
082500150723       update  lsi1s01  ;
082600150723
082700150723       ENDSR   ;
082800160330       //--------------------------------------------------------------
082900160330       //?Chiusura ORM
083000160330       //--------------------------------------------------------------
083100160330       BEGSR ChiuORM   ;
083200160330               if vs1ass<>*blanks;
083300160330                  chain (blpaas:blplnp:blpnrs:blpnsp:'M') fiar401l;
083400160330                  if %found(fiar401l);
083500160330                    dsbl4m=ar4not;
083600160330                    clear fior51ds;
083700160330                    I51POE=§B4POE;
083800160330                    I51NSR=§B4NSR;
083900160330                    I51NOR=§B4NOR;
084000160330                    I51NRV=§B4NRV;
084100160330                    I51FAR=999;
084200160330                    fior51r(fior51ds);
084300160330                  endif;
084400160330               endif;
084500160330       ENDSR   ;
084600150723      /end-free
084700150723     C*--------------------------------------------------------------*
084800150723     C* interroga bolla in partenza
084900150723     C*--------------------------------------------------------------*
085000150723     C     int_bolla     BEGSR
085100150723     C                   CLEAR                   PARA05
085200150723     C                   Z-ADD     VshAAS        P05AAS
085300150723     C                   Z-ADD     VshLNP        P05LNP
085400150723     C                   Z-ADD     VshNRS        P05NRS
085500150723     C                   Z-ADD     VshNSP        P05NSP
085600150723     C                   MOVEL     '1'           P05GIA
085700150723     C                   MOVEL     PARA05        KPJBU
085800150723     C                   CALL      'FNLS05R'
085900150723     C                   PARM                    KPJBA
086000150723     c                   endsr
086100970410     C*--------------------------------------------------------------*
086200130822     C* Decodifica i 2 riferimenti per impostare la spedizione
086300970410     C*--------------------------------------------------------------*
086400130822     C     decod_da_Rif  BEGSR
086500970410     C*
086600131016     c                   clear                   piu_bolle_Xrif    1
086700131016     c                   clear                   V2NOTE1
086800131016     c                   clear                   V2NOTE2
086900131016     C*
087000130822     c                   clear                   se_trovato        1
087100130822     c                   eval      se_trovato ='N'
087200130822     C*
087300130822     c                   if        v2rif32 <> *blank
087400131016     c     v2rif32       setll     fnblp32l
087500131016     c     v2rif32       reade     fnblp32l
087600131016     c                   dow       not %EoF(fnblp32l)
087700130822     c                   eval      se_trovato ='S'
087800131016     c     v2rif32       reade     fnblp32l
087900131016     c                   if        not %EoF(fnblp32l)
088000131016     c                   eval       piu_bolle_Xrif ='S'
088100131016     c                   end
088200130822     c                   end
088300130822     c                   end
088400130822      *
088500130822     c                   if        v2rif33 > 0 and
088600130822     c                             se_trovato ='N'
088700131016     c     v2rif33       setll     fnblp33l
088800131016     c     v2rif33       reade     fnblp33l
088900131016     c                   dow       not %Eof(fnblp33l)
089000130822     c                   eval      se_trovato ='S'
089100131016     c     v2rif33       reade     fnblp33l
089200131016     c                   if        not %EoF(fnblp33l)
089300131016     c                   eval       piu_bolle_Xrif ='S'
089400131016     c                   end
089500130822     c                   end
089600130822     c                   end
089700130822     C*
089800130822     C* Se lo ha trovato imposta imposta i campi della spedizione
089900131016     C*   ATTENZIONE potrebbero essercene più di una con lo stesso
090000131016     C* riferimento quindi segnala che ha preso l'ultima
090100130822     c                   IF        se_trovato ='S'
090200131016     c                               and v2cnsp = 0
090300130822     c                   eval      v2caas = blpaas
090400130822     c                   eval      v2clnp = blplnp
090500130822     c                   eval      v2cnrs = blpnrs
090600130822     c                   eval      v2cnsp = blpnsp
090700130822     c                   end
090800131016     C*   AVVISA SE HA trovato più bolle con lo stesso riferimento
090900131016     c                   IF         piu_bolle_Xrif ='S'
091000131016     c                   EVAL        V2NOTE1 = err(25)
091100131016     c                   EVAL        V2NOTE2 = err(26)
091200131016     c                   end
091300131016     C*
091400130822     C                   ENDSR
091500130822     C*--------------------------------------------------------------*
091600130822     C* CTR02: Controlli prima videata
091700130822     C*--------------------------------------------------------------*
091800130822     C     CTR02         BEGSR
091900130822     C*
092000130822     C*  pre- controllo bolla tramite rif.alfa o numerico
092100130822     c                   exsr      decod_da_Rif
092200130822     C*
092300970410     C* Controllo se immesso anno
092400970611     C     V2CAAS        IFEQ      0
092500970410     C                   SETON                                        2840
092600970611     C                   MOVEL     ERR(5)        $MSG
092700970410     C                   END
092800990610     C   28              GOTO      FINCT2
092900970410     C* Se anno digitato di 2 --> 4 cifre
093000970612     C                   MOVEL     V2CAAS        WSECOL            2 0
093100970410     C     WSECOL        IFEQ      0
093200970611     C     V2CAAS        IFGT      60
093300970611     C                   ADD       1900          V2CAAS
093400970410     C                   ELSE
093500970611     C                   ADD       2000          V2CAAS
093600970410     C                   END
093700970410     C                   END
093800970410     C* Controllo se immessa linea di partenza
093900970611     C     V2CLNP        IFEQ      0
094000970414     C                   SETON                                        2841
094100970611     C                   MOVEL     ERR(6)        $MSG
094200970410     C                   END
094300990610     C   28              GOTO      FINCT2
094400970410     C* Controllo esistenza lnp
094500970611     C                   Z-ADD     V2CLNP        ORGFIL
094600970410     C     ORGFIL        CHAIN     AZORG01L                           31
094700970611     C     *IN31         IFEQ      '1'
094800970410     C                   SETON                                        2841
094900970611     C                   MOVEL     ERR(7)        $MSG
095000970414     C                   END
095100990610     C   28              GOTO      FINCT2
095200970414     C* Controllo se lnp in £1
095300970611     C     V2CLNP        LOOKUP    £1                                     32
095400970414     C     *IN32         IFEQ      '0'
095500970414     C                   SETON                                        2841
095600970611     C                   MOVEL     ERR(8)        $MSG
095700970414     C                   END
095800990610     C   28              GOTO      FINCT2
095900970414     C* Controllo se immesso numero di spedizione
096000970611     C     V2CNSP        IFEQ      0
096100970414     C                   SETON                                        2843
096200970611     C                   MOVEL     ERR(9)        $MSG
096300970414     C                   END
096400990610     C   28              GOTO      FINCT2
096500970414     C* Controllo se esiste spedizione
096600970611     C                   Z-ADD     V2CAAS        KAAS
096700970611     C                   Z-ADD     V2CLNP        KLNP
096800970611     C                   Z-ADD     V2CNRS        KNRS
096900970611     C                   Z-ADD     V2CNSP        KNSP
097000970414     C     KBLP          CHAIN(N)  FNBLP01L                           31
097100970414     C     *IN31         IFEQ      '1'
097200970414     C                   SETON                                        2843
097300970611     C                   MOVEL     ERR(10)       $MSG
097400970616     C                   ELSE
097500970616     C* Controllo che la spedizione sia del cliente selezionato
097600970616     C     BLPCCM        IFNE      V2CKSC
097700970616     C     BLPKSC        ANDNE     V2CKSC
097800970616     C                   SETON                                        2843
097900980202     C                   MOVEL     ERR(17)       $MSG
098000970414     C                   END
098100970616     C                   END
098200990610     C   28              GOTO      FINCT2
098300970414     C* Controllo se bolla mamma
098400150728     C*                  Z-ADD     V2CAAS        KAAP
098500150728     C*                  Z-ADD     V2CLNP        KLNP
098600150728     C*                  Z-ADD     V2CNRS        KNRP
098700150728     C*                  Z-ADD     V2CNSP        KNSP
098800150728     C*    KBLP          CHAIN     FNLBL02L                           31
098900970414     C* Controllo se bolla figlia
099000150728     C*                  Z-ADD     V2CAAS        KAAP
099100150728     C*                  Z-ADD     V2CLNP        KLNP
099200150728     C*                  Z-ADD     V2CNRS        KNRP
099300150728     C*                  Z-ADD     V2CNSP        KNSP
099400150728     C*    KBLP          CHAIN     FNLBL01L                           31
099500150728     C*    *IN31         IFEQ      '0'
099600150728     C*                  SETON                                        284344
099700150728     C*                  MOVEL     ERR(12)       $MSG
099800150728     C*                  END
099900150728     C*  28              GOTO      FINCT2
100000980204     C* Controllo che non sia stata data per consegnata
100100150728     C*    BLPDCM        IFNE      0
100200150728     C*    BLPCCA        ANDNE     '6'
100300150728     C*    BLPDCM        ORNE      0
100400150728     C*    BLPCCA        ANDNE     '2'
100500150728     C*                  SETON                                        2843
100600150728     C*                  MOVEL     ERR(22)       $MSG
100700150728     C*                  END
100800150728     C*  28              GOTO      FINCT2
100900980204     C* Controllo che abbia il dettaglio segnacolli
101000150728     C*                  Z-ADD     V2CAAS        KAAS
101100150728     C*+                 Z-ADD     V2CLNP        KLNP
101200150728     C**                 Z-ADD     V2CNRS        KNRS
101300150728     C*                  Z-ADD     V2CNSP        KNSP
101400150728     C*    KBLT          CHAIN     FNBLT01L                           31
101500150728     C*    *IN31         IFEQ      '1'
101600150728     C*                  SETON                                        2843
101700150728     C*                  MOVEL     ERR(16)       $MSG
101800150728     C*                  END
101900150728     c                   exsr      ctr_uniS
102000150728     C   28              GOTO      FINCT2
102100980202     C* Controllo se richiesto '?'
102200980202     C     '?'           SCAN      V2CSTS                                 32
102300980202     C     *IN32         IFEQ      '1'
102400980202     C                   MOVE      LAB(XY)       DI1LAB
102500980202     C                   MOVEL     FNLSI1        KPJBU
102600980203     C                   CALL      'FNLSI1R5'
102700980202     C                   PARM                    KPJBA
102800980204     C                   MOVEL     KPJBU         FNLSI1
102900980204     C                   MOVEL     DI1CEV        V2CCOD
103000980202     C                   MOVEL     DI1STS        V2CSTS
103100980202     C                   END
103200980202     C* evento obbligatoria
103300980202     C     V2CCOD        IFEQ      *BLANKS
103400980202     C     V2CSTS        ANDEQ     *BLANKS
103500970721     C                   SETON                                        2844
103600980202     C                   MOVEL     ERR(21)       $MSG
103700970721     C                   END
103800990610     C   28              GOTO      FINCT2
103900980202     C* evento esistente
104000980202     C                   MOVEL     V2CSTS        WEVE              6
104100980202     C                   MOVE      V2CCOD        WEVE
104200030116     C                   Z-ADD     1             YY                5 0
104300980202     C     LAB(XY)       IFEQ      'S'
104400980202     C     WEVE          LOOKUP    ELB(YY)                                32
104500980202     C                   MOVEL     DLB(YY)       DSEM
104600980202     C                   ELSE
104700980202     C     WEVE          LOOKUP    EVG(YY)                                32
104800980202     C                   MOVEL     DVG(YY)       DSEM
104900980203     C                   MOVEL     §EMDES        V2DCOD
105000980202     C                   END
105100980204     C     *IN32         IFEQ      '0'
105200980202     C                   SETON                                        2844
105300980202     C                   MOVEL     ERR(19)       $MSG
105400980202     C                   END
105500990610     C   28              GOTO      FINCT2
105600980203     C* Se bolla in assegnato no IDD Totale definitivo
105700150728     C*                  Z-ADD     1             X
105800150728     C*    BLPCBO        LOOKUP    C3A(X)                                 32
105900150728     C*                  MOVEL     TPO(X)        WTPO              1
106000150728     C*    *IN32         IFEQ      '1'
106100150728     C*    WTPO          ANDEQ     'A'
106200150728     C*    §EMASS        ANDNE     'S'
106300160405     C*                  SETON                                        2843
106400150728     C*                  MOVEL     ERR(20)       $MSG
106500150728     C*                  END
106600150728     C*  28              GOTO      FINCT2
106700970414     C* Controllo che non sia stata data per partita
106800150728     C*    BLPDT1        IFNE      0
106900150728     C*    §EMTRA        ANDNE     'S'
107000150728     C*                  SETON                                        2843
107100150728     C*                  MOVEL     ERR(13)       $MSG
107200150728     C*                  END
107300150728     C*  28              GOTO      FINCT2
107400970414     C* Controllo che non sia stata trasmessa in sede
107500150728     C*    BLPDT2        IFNE      0
107600150728     C*    §EMTRA        ANDNE     'S'
107700150728     C*                  SETON                                        2843
107800150728     C*                  MOVEL     ERR(14)       $MSG
107900150728     C*                  END
108000150728     C*  28              GOTO      FINCT2
108100970414     C* Controllo che non sia stata trasmessa al cliente
108200150728     C*    BLPDT3        IFNE      0
108300150728     C*    §EMTRA        ANDNE     'S'
108400150728     C*                  SETON                                        2843
108500150728     C*                  MOVEL     ERR(15)       $MSG
108600150728     C*                  END
108700150728     C*  28              GOTO      FINCT2
108800990610      * Controllo C.A.
108900150728     C*    §EMCAC        IFEQ      'S'
109000040715      *
109100150728     c*                  clear                   fidn12ds
109200150728     c*                  eval      i12aas = V2CAAS
109300150728     c*                  eval      i12lnp = V2CLNP
109400150728     c*                  eval      i12nrs = V2CNRS
109500150728     c*                  eval      i12nsp = V2CNSP
109600150728     c*                  eval      i12fel = 'E'
109700150728     c*                  eval      i12fan = 'E'
109800150728     c*                  eval      i12fch = 'E'
109900150728     c*                  call      'FIDN12R'
110000150728     c*                  parm                    fidn12ds
110100040715      **
110200040715      * se c'è almeno una C.A. aperta
110300150728     c*    o12nca        ifgt      0
110400150728     C*                  SETON                                        284331
110500150728     C*                  MOVEL     ERR(24)       $MSG
110600150728     c*                  end
110700040715      **
110800150728     C*                  ENDIF
110900040715      **
111000160307     c                   eval      ctrvid=*on
111100150728     c                   exsr      ctr_uniE
111200990610     C   28              GOTO      FINCT2
111300040715      **
111400980204     C* Controllo se l'evento è già stato inviato
111500980204     C                   Z-ADD     V2CAAS        KAA2
111600980204     C                   MOVEL     §EMCOD        KCEV
111700980204     C                   MOVEL     ' '           WDAFOR            1
111800990810     C     KEVB          CHAIN     FNEVB04L                           31
111900980204     C* Evento già immesso
112000980204     C     *IN31         IFEQ      '0'
112100980204     C* se non è possibile forzare do errore
112200980204     C     §EMFOR        IFNE      'S'
112300980204     C                   SETON                                        2843
112400980204     C                   MOVEL     ERR(23)       $MSG
112500980204     C                   ELSE
112600980204     C                   MOVEL     'F'           WDAFOR
112700980204     C                   END
112800980204     C                   END
112900990610     C   28              GOTO      FINCT2
113000970414     C*
113100970612     C     FINCT2        ENDSR
113200150724     C*--------------------------------------------------------------*
113300150728     C*    Controlli per il caricamento e gestione sfl
113400150724     C*--------------------------------------------------------------*
113500150728     C     ctr_sfl       BEGSR
113600150724     c                   clear                   $msg
113700150724     c                   setoff                                       28
113800150728     c                   exsr      ctr_uniS
113900150728     c                   if        *in28
114000150728     c                   leavesr
114100150728     c                   endif
114200150728     c                   exsr      ctr_uniE
114300150728     c                   if        *in28
114400150728     c                   leavesr
114500150728     c                   endif
114600150724     c                   clear                   wsispu            1
114700150724     c* controllo che la spedizione non abbia spunte
114800150728     c     kblt          setll     fnblt01l
114900150728     c     kblt          reade     fnblt01l                               99
115000150724    2c                   dow       not *in99
115100150724     c
115200150724     c* Cerco una qualsiasi spunta
115300150724     c     kbrv          chain     fnbrv07l
115400150724    3c                   if        %found(fnbrv07l)
115500150724     c                   eval      wsispu='S'
115600150724     c                   seton                                        99
115700150724   x3c                   else
115800150728     c     kblt          reade     fnblt01l                               99
115900150724    3c                   endif
116000150724    2c                   enddo
116100150724     c*
116200150724    2c                   if        wsispu='S'
116300150724     c                   seton                                        28
116400150724     C                   MOVEL     ERR(29)       $MSG
116500150724     c                   leavesr
116600150724     c                   endif
116700150724     c
116800150724     c
116900150724     c                   endsr
117000150728     C*--------------------------------------------------------------*
117100150728     C*    Controlli unici per spedizione
117200150728     C*--------------------------------------------------------------*
117300150728     C     ctr_uniS      BEGSR
117400150728     C* Controllo se bolla figlia
117500150728     C     KBLP          CHAIN     FNLBL01L
117600150728     C                   if        %found(fnlbl01l)
117700150728     C                   SETON                                        28
117800150728     C                   SETON                                          4344
117900150728     C                   MOVEL     ERR(12)       $MSG
118000150728     c                   leavesr
118100150728     C                   END
118200150728     C* Controllo che non sia stata data per consegnata
118300150728     C     BLPDCM        IFNE      0
118400150728     C     BLPCCA        ANDNE     '6'
118500150728     C     BLPDCM        ORNE      0
118600150728     C     BLPCCA        ANDNE     '2'
118700150728     C                   SETON                                        28
118800150728     C                   SETON                                        43
118900150728     C                   MOVEL     ERR(22)       $MSG
119000150728     c                   leavesr
119100150728     C                   END
119200150728     C* Controllo che abbia il dettaglio segnacolli
119300150728     c     kblt          setll     fnblt01l
119400150728     C                   IF        not %equal(fnblt01l)
119500150728     C                   SETON                                        28
119600150728     C                   SETON                                        43
119700150728     C                   MOVEL     ERR(16)       $MSG
119800150728     c                   leavesr
119900150728     C                   END
120000150728     c                   endsr
120100150728     C*--------------------------------------------------------------*
120200150728     C*    Controlli unici legati all'evento
120300150728     C*--------------------------------------------------------------*
120400150728     C     ctr_uniE      BEGSR
120500150728     C* Se bolla in assegnato no IDD
120600160307     c* Solo se vengo da controlli videata
120700160317     c*                  if        ctrvid=*on
120800160317     C*                  Z-ADD     1             X
120900160317     C*    BLPCBO        LOOKUP    C3A(X)                                 32
121000160317     C*                  MOVEL     TPO(X)        WTPO              1
121100160317     C*    *IN32         IFEQ      '1'
121200160317     C*    WTPO          ANDEQ     'A'
121300160317     C*    §EMASS        ANDNE     'S'
121400160317     c*                  eval      ktrc='M'
121500160317     c*    kar4          chain     fiar401l
121600160317     c*                  if        %found(fiar401l)
121700160317     c*                  eval      dsbl4m=ar4not
121800160317     c*    korm          chain     fnorm01l
121900160317     c*                  if        %found(fnorm01l) and ormfao<>999
122000160317     C*                  SETON                                        28
122100160317     C*                  SETON                                        43
122200160317     C*                  MOVEL     ERR(20)       $MSG
122300160317     c*                  leavesr
122400160317     c*                  endif
122500160317     c*                  endif
122600160317     C*                  END
122700160317     c*                  endif
122800150728     C* Controllo che non sia stata data per partita
122900150728     C     BLPDT1        IFNE      0
123000150728     C     §EMTRA        ANDNE     'S'
123100150728     C                   SETON                                        2843
123200150728     C                   MOVEL     ERR(13)       $MSG
123300150728     c                   leavesr
123400150728     C                   END
123500150728     C* Controllo che non sia stata trasmessa in sede
123600150728     C     BLPDT2        IFNE      0
123700150728     C     §EMTRA        ANDNE     'S'
123800150728     C                   SETON                                        2843
123900150728     C                   MOVEL     ERR(14)       $MSG
124000150728     c                   leavesr
124100150728     C                   END
124200150728     C* Controllo che non sia stata trasmessa al cliente
124300150728     C     BLPDT3        IFNE      0
124400150728     C     §EMTRA        ANDNE     'S'
124500150728     C                   SETON                                        2843
124600150728     C                   MOVEL     ERR(15)       $MSG
124700150728     c                   leavesr
124800150728     C                   END
124900150728      * Controllo C.A.
125000150728      *
125100150728     C     §EMCAC        IFEQ      'S'
125200150728     c                   clear                   fidn12ds
125300150728     c                   eval      i12aas = blpaAS
125400150728     c                   eval      i12lnp = blpLNP
125500150728     c                   eval      i12nrs = blpNRS
125600150728     c                   eval      i12nsp = blpNSP
125700150728     c                   eval      i12fel = 'E'
125800150728     c                   eval      i12fan = 'E'
125900150728     c                   eval      i12fch = 'E'
126000150728     c                   call      'FIDN12R'
126100150728     c                   parm                    fidn12ds
126200150728      **
126300150728      * se c'è almeno una C.A. aperta
126400150728     c     o12nca        ifgt      0
126500150728     C                   SETON                                        28
126600150728     C                   SETON                                          4331
126700150728     C                   MOVEL     ERR(24)       $MSG
126800150728     c                   leavesr
126900150728     c                   end
127000150728     C                   ENDIF
127100150728     c                   endsr
127200970612     C*--------------------------------------------------------------*
127300970612     C* CALPGM: Eseguo Call a pgm
127400970612     C*--------------------------------------------------------------*
127500970612     C     CALPGM        BEGSR
127600970612     C*
127700970612     C*  Imposto DS per richiamo altri pgm
127800970612     C                   CLEAR                   FNLSI1
127900150723     c                   if        wtpvid='S'
128000150723     C                   clear                   DI1FTX
128100150723     C                   MOVEL     V1CKSC        DI1KSC
128200150723     C                   MOVEL     VhdKSC        DI1DEC
128300150723     C                   MOVEL     VshLNP        DI1LNP
128400150723     C                   MOVEL     VshNRS        DI1NRS
128500150723     C                   MOVEL     VshNSP        DI1NSP
128600150723     C                   MOVEL     VshAAS        DI1AAS
128700150723     C                   MOVEL     VshAAS        DI1DSP
128800150723     C                   MOVE      BLPMGS        DI1DSP
128900150723     C                   MOVE      LAB(XY)       DI1LAB
129000150723     C                   MOVE      wsts          DI1STS
129100150723     C                   MOVE      wcev          DI1CEV
129200160209     C**                 MOVEL     wsts          WEVE              6
129300160209     C**                 MOVE      wcev          WEVE
129400150723
129500160209     C***                Z-ADD     1             YY                5 0
129600160209     C**   LAB(XY)       IFEQ      'S'
129700160209     C***  WEVE          LOOKUP    ELB(YY)                                32
129800160209     C***                MOVEL     DLB(YY)       DSEM
129900160209     C***                ELSE
130000160209     C***  WEVE          LOOKUP    EVG(YY)                                32
130100160209     C***                MOVEL     DVG(YY)       DSEM
130200160209     C***                END
130300150723
130400150723     C                   MOVE      §EMDES        DI1DEV
130500150723     C                   MOVE      §EMDES        DI1DEV
130600150723     C                   MOVE      §EMCOD        DI1C2A
130700150723     C                   MOVE      §EMCCA        DI1CCA
130800150723     C                   MOVE      §EMBLP        DI1BLP
130900150723     C                   MOVE      §EMBLT        DI1BLT
131000150723     C                   MOVE      §EMFOR        DI1FRZ
131100150723     C                   MOVE      §EMTOT        DI1TOT
131200150723     C                   MOVE      BLPNCL        DI1NCL
131300150723     c                   else
131400980203     C                   MOVEL     V2CFTX        DI1FTX
131500980203     C                   MOVEL     V2CKSC        DI1KSC
131600970612     C                   MOVEL     V2DKSC        DI1DEC
131700970612     C                   MOVEL     V2CLNP        DI1LNP
131800970612     C                   MOVEL     V2CNRS        DI1NRS
131900970612     C                   MOVEL     V2CNSP        DI1NSP
132000970612     C                   MOVEL     V2CAAS        DI1AAS
132100970612     C                   MOVEL     V2CAAS        DI1DSP
132200970612     C                   MOVE      BLPMGS        DI1DSP
132300970616     C                   MOVE      BLPMGS        DI1DSP
132400971007     C                   MOVE      LAB(XY)       DI1LAB
132500150723     C                   MOVE      V2CSTS        DI1STS
132600150723     C                   MOVE      V2CCOD        DI1CEV
132700980202     C                   MOVE      §EMDES        DI1DEV
132800980203     C                   MOVE      §EMDES        DI1DEV
132900980203     C                   MOVE      §EMCOD        DI1C2A
133000980203     C                   MOVE      §EMCCA        DI1CCA
133100980203     C                   MOVE      §EMBLP        DI1BLP
133200980203     C                   MOVE      §EMBLT        DI1BLT
133300980203     C                   MOVE      §EMFOR        DI1FRZ
133400980203     C                   MOVE      §EMTOT        DI1TOT
133500980204     C                   MOVE      WDAFOR        DI1FOR
133600980204     C                   MOVE      BLPNCL        DI1NCL
133700150723     c                   endif
133800970612     C                   MOVEL     FNLSI1        KPJBU
133900980202     C* Richiamo pgm gestione dettaglio segnacolli
134000980202     C     §EMTOT        IFNE      'S'
134100980202     C                   CALL      'FNLSI1R2'
134200980202     C                   PARM                    KPJBA
134300980202     C                   MOVEL     KPJBU         FNLSI1
134400980202     C                   END
134500980202     C* Se ho digitato F3 esco
134600980202     C     DI1CMD        IFEQ      '03'
134700980202     C                   MOVEL     'S'           WFINE
134800980202     C                   GOTO      FINRIC
134900980202     C                   END
135000980204     C* Se ho digitato F12 azzero campo scelta
135100980204     C     DI1CMD        IFEQ      '12'
135200980204     C                   GOTO      FINRIC
135300980204     C                   END
135400980202     C* Richiamo pgm gestione terza videata: SPEDIZIONE
135500970612     C                   CALL      'FNLSI1R1'
135600970612     C                   PARM                    KPJBA
135700970617     C                   MOVEL     KPJBU         FNLSI1
135800970612     C*
135900970612     C* Se ho digitato F3 esco
136000970612     C                   SELECT
136100970612     C     DI1CMD        WHENEQ    '03'
136200970619     C                   MOVEL     'S'           WFINE
136300970619     C* Controllo se devo richiamare TRTC81
136400970619     C     WRIC81        IFEQ      'S'
136500970619     C                   EXSR      CALL81
136600970619     C                   END
136700970612     C* Se ho digitato F12 azzero campo scelta
136800970612     C     DI1CMD        WHENEQ    '12'
136900970616     C* CONFERMA
137000970616     C     DI1CMD        WHENEQ    '06'
137100970617     C                   MOVEL     'S'           WRIC81
137200970616     C* Aggiorno BLP/BLT
137300980202     C     §EMBLP        IFEQ      'S'
137400970612     C                   CALL      'FNLSI1R3'
137500970617     C                   PARM                    KPJBA
137600970616     C                   END
137700080421      * l'EDIVAC viene sostituito con il TIVGD Download standard
137800080421     C* Scrivo EDVAC --> TIVGD00F
137900970612     C                   CALL      'FNLSI1R4'
138000970612     C                   PARM                    KPJBA
138100970616     C                   ENDSL
138200970612     C*
138300980202     C     FINRIC        ENDSR
138400970617     C*--------------------------------------------------------------*
138500980126     C* CALL81: PER E.D.I. RICHIAMO PGM TRTC81C
138600970617     C*--------------------------------------------------------------*
138700970617     C     CALL81        BEGSR
138800970617     C*
138900980126      * CALL A TRTC81C PER E.D.I.
139000980126     C     §PTFTT        IFNE      'S'
139100061124     C     §PTFTT        ANDNE     'P'
139200980126     C                   MOVEL     'S'           WCAL81            1
139300080421      *
139400080421      *  Scrive IFTSTA x EDI:
139500080421      * ? il TRTC81C  attiva giro di trasm con SNDIFTSTA  ?
139600080421     C                   MOVEL     *all'0'       Elab_Cliente
139700080421     C                   MOVE      CKS(XX)       Elab_Cliente
139800080421     c                   eval      Tipo_Scarico = CSD(XX)
139900080421      *
140000080421     c                   eval      Tipo_File = 'VC'
140100110630     C                   CALL      'TRTCT81C'
140200080421     c                   parm                    Elab_Cliente      8
140300080422     c                   parm                    Tipo_File         2
140400080421     c                   parm                    Tipo_Scarico      2
140500980126      *
140600980126     C                   END
140700980126      *
140800970617     C                   ENDSR
140900970410     C*--------------------------------------------------------------*
141000970410     C* *INZSR: Operazioni iniziali
141100970410     C*--------------------------------------------------------------*
141200970410     C     *INZSR        BEGSR
141300970410     C*
141400970410     C     *ENTRY        PLIST
141500970410     C                   PARM                    KPJBA
141600990223     C* Richiamo xparut
141700970410     C                   Z-ADD     1             CODUT
141800970410     C                   CALL      'X§PARUT'
141900970410     C                   PARM                    UTEDSE
142000970410     C                   MOVEL     REC80         CNCR80
142100970410     C                   MOVEL     RAGUT         RSUT
142200030924      *
142300030924      *  In base all'utente prende il P.O.
142400030924     C                   CLEAR                   Tibs36Ds
142500030924     c                   EVAL      I36ute = Knmus
142600030924     c                   EVAL      I36Tla = 'L'
142700030924     C                   CALL      'TIBS36R'
142800030924     C                   PARM                    Tibs36Ds
142900030924      *
143000970612     C*--- RICERCA CAPOCONTI------------------------------------------*
143100030116     C                   DO        50            X                 5 0
143200970612     C                   MOVE      TCU(X)        TCUDS
143300970612     C     F56           IFEQ      'CG'
143400970612     C     F4            ANDEQ     '1'
143500970612     C     F3            ANDEQ     '0'
143600970612     C                   Z-ADD     KCU(X)        CLIITA            4 0
143700970612     C                   END
143800970612     C                   END
143900970612     C*--- RICERCA CAPOCONTI------------------------------------------*
144000970410      *  Definizione chiavi
144100970410     C     KBLP          KLIST
144200970410     C                   KFLD                    KAAS
144300970410     C                   KFLD                    KLNP
144400970410     C                   KFLD                    KNRS
144500970410     C                   KFLD                    KNSP
144600970410     C     KBLT          KLIST
144700970410     C                   KFLD                    KAAS
144800970410     C                   KFLD                    KLNP
144900970410     C                   KFLD                    KNRS
145000970410     C                   KFLD                    KNSP
145100970410     C     KLBL1         KLIST
145200970410     C                   KFLD                    KAAP
145300970410     C                   KFLD                    KLPP
145400970410     C                   KFLD                    KNRP
145500970410     C                   KFLD                    KNSP
145600970410     C     KLBL2         KLIST
145700970410     C                   KFLD                    KAAN
145800970410     C                   KFLD                    KLPN
145900970410     C                   KFLD                    KNRN
146000970410     C                   KFLD                    KNSN
146100970410     C     KTAB1         KLIST
146200970410     C                   KFLD                    KKUT
146300970410     C                   KFLD                    KCOD
146400970410     C     KTAB2         KLIST
146500970410     C                   KFLD                    KKUT
146600970410     C                   KFLD                    KCOD
146700970410     C                   KFLD                    KKEY
146800970612     C     KACO          KLIST
146900970612     C                   KFLD                    KKUT
147000970612     C                   KFLD                    KKCC
147100970612     C                   KFLD                    KKSC
147200980204     C     KEVB          KLIST
147300980204     C                   KFLD                    KAA2
147400980204     C                   KFLD                    KLNP
147500980204     C                   KFLD                    KNRS
147600980204     C                   KFLD                    KNSP
147700980204     C                   KFLD                    KCEV
147800080918     C     KTBE1         KLIST
147900080918     C                   KFLD                    TBECOD
148000080918     C                   KFLD                    TBEKE1
148100150415     C     Kedtab        KLIST
148200150415     C                   KFLD                    tabCOD
148300150415     C                   KFLD                    tabKEY
148400150724     C     KBrv          KLIST
148500150724     C                   KFLD                    bltfls
148600150724     C                   KFLD                    bltlna
148700150724     C                   KFLD                    bltNRS
148800150724     C                   KFLD                    bltnsc
148900160308     C     Kar4          KLIST
149000160308     C                   KFLD                    KAAS
149100160308     C                   KFLD                    KLNP
149200160308     C                   KFLD                    KNRS
149300160308     C                   KFLD                    KNSP
149400160308     C                   KFLD                    Ktrc
149500160308     C     Korm          KLIST
149600160308     C                   KFLD                    §B4POE
149700160308     C                   KFLD                    §B4NSR
149800160308     C                   KFLD                    §B4NOR
149900160308     C                   KFLD                    §B4NRV
150000970410      *  Definizione variabili
150100980204     C     *LIKE         DEFINE    EVBAAS        KAA2
150200980204     C     *LIKE         DEFINE    EVBCEV        KCEV
150300970410     C     *LIKE         DEFINE    BLPAAS        KAAS
150400970410     C     *LIKE         DEFINE    BLPLNP        KLNP
150500970410     C     *LIKE         DEFINE    BLPNRS        KNRS
150600970410     C     *LIKE         DEFINE    BLPNSP        KNSP
150700970410     C     *LIKE         DEFINE    LBLAAP        KAAP
150800970410     C     *LIKE         DEFINE    LBLLPP        KLPP
150900970410     C     *LIKE         DEFINE    LBLNRP        KNRP
151000970410     C     *LIKE         DEFINE    LBLAAN        KAAN
151100970410     C     *LIKE         DEFINE    LBLLPN        KLPN
151200970410     C     *LIKE         DEFINE    LBLNRN        KNRN
151300970410     C     *LIKE         DEFINE    LBLNSN        KNSN
151400970410     C     *LIKE         DEFINE    TBLKUT        KKUT
151500970410     C     *LIKE         DEFINE    TBLCOD        KCOD
151600970410     C     *LIKE         DEFINE    TBLKEY        KKEY
151700970611     C     *LIKE         DEFINE    TABCOD        KCODE
151800970612     C     *LIKE         DEFINE    ACOKCC        KKCC
151900970612     C     *LIKE         DEFINE    ACOKSC        KKSC
152000970410      *
152100970410      *  Reperisco la data del giorno
152200970410     C                   TIME                    WHHDAT           14 0
152300970414     C                   MOVE      WHHDAT        G02DAT
152400970414     C                   MOVEL     *BLANK        G02ERR
152500970414     C                   CALL      'XSRDA8'
152600970414     C                   PARM                    WLBDA8
152700970414     C                   Z-ADD     G02INV        OGGI              8 0
152800970611     C*
152900970611     C* Carico dati DS partner in schiera
153000970612     C                   OPEN      EDTAB01L
153100150415     C                   Z-ADD     0             X
153200970611     C                   MOVEL     'PT'          KCODE
153300970616     C     KCODE         CHAIN     EDTAB01L                           30
153400970611     C     *IN30         DOWEQ     '0'
153500970611     C     TABFLG        IFEQ      ' '
153600970611     C                   ADD       1             X
153700970611     C                   MOVEL     TABUNI        DSPT
153800150415     C                   MOVEL     TABKEY        KEYPT(X)
153900970611     C                   MOVEL     §PTKSC        KSC(X)
154000970616     C                   MOVEL     §PTLNP        LNP(X)
154100970616     C                   MOVEL     §PTNRS        NRS(X)
154200970611     C                   END
154300970616     C     KCODE         READE     EDTAB01L                               30
154400970611     C                   END
154500970616     C*
154600150415     C* x evitare che a causa di un disallineamento imprevisto
154700150415     C*  la PU possa essere disallineata alla PT  eseguo un caricamento
154800150415     C*  differente.
154900150415     c                   clear                   lab
155000150415     c                   do        x             pux               5 0
155100150415     C                   MOVEL     'PU'          tabCOD
155200150415     C                   MOVEL     KEYPT(pux)    tabKEY
155300150415     C     Kedtab        CHAIN     EDTAB01L
155400150415     c                   if        %Found(edtab01l) and tabflg =' '
155500150415      *  Lista Labarta
155600150415     C                   MOVEL     TABUNI        DSPU
155700150415     C                   MOVEL     §PULAB        LAB(pux)
155800150415     c                   end
155900150415     c                   enddo
156000150415      *
156100150415      **** vecchio caricamento
156200150415     C****               Z-ADD     0             X
156300150415     C****               MOVEL     'PU'          KCODE
156400150415     C**** KCODE         CHAIN     EDTAB01L                           30
156500150415     C**** *IN30         DOWEQ     '0'
156600150415     C**** TABFLG        IFEQ      ' '
156700150415     C****               ADD       1             X
156800150415     C****               MOVEL     TABUNI        DSPU
156900150415     C****               MOVEL     §PULAB        LAB(X)
157000150415     C****               END
157100150415     C**** KCODE         READE     EDTAB01L                               30
157200150415     C****               END
157300150415      *
157400150415     C*  Carico in schiera codici collegati - CL -
157500150415     C                   Z-ADD     0             X
157600150415     C                   MOVEL     'CL'          KCODE
157700150415     C     KCODE         CHAIN     EDTAB01L                           30
157800150415     C     *IN30         DOWEQ     '0'
157900150415     C     TABFLG        IFEQ      ' '
158000150415     C                   ADD       1             X
158100150415     C                   MOVEL     TABUNI        DSCL
158200150415     C                   MOVEL     §CLKSC        clKSC(X)
158300150415     C                   MOVEL     §CLLNP        clLNP(X)
158400150415     C                   END
158500150415     C     KCODE         READE     EDTAB01L                               30
158600150415     C                   END
158700980202     C*
158800980202     C* Carico in schiera tabella eventi manuali
158900150723     c                   clear                   wcev
159000150723     c                   clear                   wsts
159100980202     C                   Z-ADD     0             X
159200030116     C                   Z-ADD     0             X1                5 0
159300980202     C                   MOVEL     'EM'          KCODE
159400980202     C     KCODE         CHAIN     EDTAB01L                           30
159500980202     C     *IN30         DOWEQ     '0'
159600980202     C     TABFLG        IFEQ      ' '
159700980202     C                   MOVEL     TABUNI        DSEM
159800980202     C     §EMLAB        IFEQ      'S'
159900980202     C                   ADD       1             X
160000980204     C                   MOVEL     TABKEY        ELB(X)
160100980202     C                   MOVEL     TABUNI        DLB(X)
160200980202     C                   END
160300980202     C     §EMVGL        IFEQ      'S'
160400980202     C                   ADD       1             X1
160500980204     C                   MOVEL     TABKEY        EVG(X1)
160600980202     C                   MOVEL     TABUNI        DVG(X1)
160700980202     C                   END
160800150723     c* memorizzo il codice dell'evento corrispondente alla chiusura bolla
160900150723     c* per merce mai affidata
161000150723     c                   if        §emmai='S'
161100150723     c                   movel     tabkey        wsts
161200150723     c                   eval      wcev=%subst(tabkey:4:3)
161300160209     c                   eval      s_dsem=dsem
161400150723     c                   endif
161500980202     C     KCODE         READE     EDTAB01L                               30
161600980202     C                   END
161700980202     C                   END
161800970611     C* close EDTAB
161900970611     C                   CLOSE     EDTAB01L
162000970611     C*
162100970410     C* Eseguo caricamento tabella 3K
162200160407     C****               OPEN      TABEL00F
162300970410     C                   Z-ADD     0             X
162400970410     C                   Z-ADD     1             KKUT
162500970410     C                   MOVEL     '3K'          KCOD
162600970410     C     KTAB1         CHAIN     TABEL                              30
162700970410     C     *IN30         DOWEQ     '0'
162800030213     C     X             ANDLT     9990
162900970410     C     TBLFLG        IFEQ      *BLANKS
163000970410     C                   ADD       1             X
163100970617     C                   MOVEL     TBLUNI        DS3K
163200970617     C                   MOVEL     TBLKEY        C3K(X)
163300970617     C                   MOVEL     §3KCKS        CKS(X)
163400010725     C                   MOVEL     §3KCSD        CSD(X)
163500970410     C                   END
163600970410     C     KTAB1         READE     TABEL                                  30
163700970410     C                   END
163800970715     C* Eseguo caricamento tabella 3A
163900970721     C                   Z-ADD     0             X
164000970715     C                   MOVEL     '3A'          KCOD
164100970715     C     KTAB1         CHAIN     TABEL                              30
164200970715     C     *IN30         DOWEQ     '0'
164300970721     C     X             ANDLT     500
164400970715     C     TBLFLG        IFEQ      *BLANKS
164500970715     C                   ADD       1             X
164600970715     C                   MOVEL     TBLUNI        DS3A
164700970715     C                   MOVEL     TBLKEY        C3A(X)
164800970715     C                   MOVEL     §3ATB1        TPO(X)
164900970715     C                   END
165000970715     C     KTAB1         READE     TABEL                                  30
165100970715     C                   END
165200970701     C*
165300970701     C* Eseguo caricamento da tabella 7J codici pistole fittizie
165400970701     C                   MOVEA     *HIVAL        C7J
165500970701     C                   Z-ADD     0             X
165600970701     C                   Z-ADD     1             KKUT
165700970701     C                   MOVEL     '7J'          KCOD
165800970701     C     KTAB1         CHAIN     TABEL                              30
165900970701     C     *IN30         DOWEQ     '0'
166000970721     C     X             ANDLT     500
166100970701     C     TBLFLG        IFEQ      *BLANKS
166200970701     C                   MOVEL     TBLUNI        DS7J
166300970912     C     §7JRPS        IFEQ      'A'
166400970701     C                   ADD       1             X
166500970701     C                   MOVEL     TBLKEY        C7J(X)
166600970701     C                   END
166700970701     C                   END
166800970701     C     KTAB1         READE     TABEL                                  30
166900970701     C                   END
167000150728
167100150728     C                   MOVEL     '3I'          Kcod
167200150728     C                   MOVEL     '1       '    kKEY
167300150728     c                   clear                   ds3i
167400150728     C     KTAB2         CHAIN     TABEL00F
167500150728     c                   if        %found(tabel00f)
167600150728     C                   MOVEL     TBLUNI        DS3I
167700150728     c                   endif
167800970410     C* Eseguo caricamento £1
167900020506     c                   clear                   trul06ds
168000020506     c                   eval      d06cod = '£1'
168100030924     c                   movel     o36pou        d06key
168200020506     c                   movel     trul06ds      kpjbu
168300020506     c                   call      'TRUL06R'
168400020506     c                   parm                    kpjba
168500020506     c                   movel     kpjbu         trul06ds
168600020506     c                   movea     lin           £1
168700150724     c* terminal di partenza del p.o. utente
168800150724     c                   clear                   fnlv55ds
168900150724     C                   MOVEL     'P'           D55TPT
169000150724     C                   MOVEL     simpou        D55LIN
169100150724     C                   z-add     oggi          D55drf
169200150724     C                   CALL      'FNLV55R'
169300150724     C                   PARM                    fnlv55ds
169400150724     c                   z-add     d55tfp        wtfp
169500020912      *
169600971008     C* Imposto a 'N' flag richiamo TRTC81C
169700971008     C                   MOVEL     'N'           WCAL81            1
169800970410     C* Eseguo close del file
169900160407     C****               CLOSE     TABEL00F
170000970410     C*
170100970410     C                   ENDSR
170200080930      *------------------------------------------------------------------------*
170300080930      * Routine di controllo x eventuali Errori
170400080930      *------------------------------------------------------------------------*
170500080930     C     *PSSR         BEGSR
170600080930      *
170700080930      * Comunque eseguo x rilasciare il TIVGD.
170800080930      *? Infine elimino il blocco elaborazione TIVGD x tipo file qui.  ?
170900080930     C                   clear                   trul47ds
171000080930     C                   eval      d47opz  = 'F'
171100080930     C                   eval      d47tip  = 'VC'
171200080930     C                   call      'TRUL47R'
171300080930     C                   parm                    trul47ds
171400080930      *
171500080930     C                   ENDSR     '*CANCL'
171600080930     C*------------------------------------------------------------------------*
171700971008**
171800970701Immettere codice cliente                                                      01
171900970701Codice cliente inesistente                                                    02
172000060811Codice cliente non presente in tabella 3K o PT                                03
172100970701Un altro utente sta chiudendo le spedizioni del cliente                       04
172200970701Immettere anno della spedizione                                               05
172300970701Immettere linea di partenza della spedizione                                  06
172400970701Linea di partenza inesistente                                                 07
172500970701Linea di partenza non gestita su questo as                                    08
172600970701Immettere numero della spedizione                                             09
172700970701Spedizione inesistente                                                        10
172800980206Bolla mamma: impossibile gestirla                                             11
172900980206Bolla figlia: impossibile gestirla                                            12
173000970701Spedizione già trasmessa alla filiale di arrivo                               13
173100970701Spedizione già trasmessa alla sede                                            14
173200970701Spedizione già trasmessa al cliente                                           15
173300980202Spedizione senza colli: impossibile eseguire chiusura                         16
173400980202Spedizione non appartenente al cliente selezionato                            17
173500980202Il codice selezionato deve appartenere ad un partner                          18
173600980202Codice evento inesistente                                                     19
173700160308Bolla in assegnato: occorre prima verificare ed annullare l'ORM               20
173800980202Codice evento obbligatorio                                                    21
173900980204Spedizione già consegnata                                                     22
174000980204Evento già inviato impossibile rinviarlo                                      23
174100990610Esistono C.A. aperte per la spedizione, codice evento non valido              24
174200131016 ATTENZIONE: più bolle con stesso riferimento!!                               25
174300131016 VERIFICARE se la Spedizione è quella corretta.                               26
174400150723Nessun dato da visualizzare per la selezione richiesta                        27
174500150728                                                                              28
174600150724Spedizione non gestibile:presenti spunte                                      29
174700150728Attenzione: caricate solo 9.999 bolle ma ne sono presenti ancora              30
174800150909Non trovato Evento per Chiusura per Merce mai Affidata. Contattare il CED (EM)31
174900160408Immettere una selezione                                                       32
