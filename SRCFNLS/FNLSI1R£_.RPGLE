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
003200970410     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
003300970414     F*--------
003400970414     FFNLSI1D   CF   E             WORKSTN
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
005100970611     D KSC             S              7  0 DIM(500)
005200970616     D LNP             S              3  0 DIM(500)
005300970616     D NRS             S              2  0 DIM(500)
005400970616     D LAB             S              1    DIM(500)
005500970617     D TPC             S              1    DIM(500)
005600980202     D* Schiera codici tabella EM
005700980202     D ELB             S              6    DIM(500)
005800980202     D DLB             S             90    DIM(500)
005900980202     D EVG             S              6    DIM(500)
006000980202     D DVG             S             90    DIM(500)
006100970410     D* Caricamento £1
006200020506     D*UNI             S              3    DIM(29)
006300020506     D £1              S              3  0 DIM(30)
006400970414     D* ERRORI
006500990610     D ERR             S             78    DIM(24) CTDATA PERRCD(1)
006600970410     D*--------------------------------------------------------------*
006700970410     D* DS
006800970410     D*--------------------------------------------------------------*
006900030924     D Tibs36ds      E DS
007000970612     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
007100970612     D  TCU                  398    697
007200970612     D                                     DIM(50)
007300970612     D  KCU                  698    847P 0
007400970612     D                                     DIM(50)
007500970612     D                                     PACKEVEN
007600970612     D TCUDS           DS
007700970612     D  F3                     3      3
007800970612     D  F4                     4      4
007900970612     D  F56                    5      6
008000970612     D CNCR80        E DS
008100020506     d trul06ds      e ds
008200020506     d  lin                    1     90  0 dim(30)
008300970612     D* DS  appoggio x cmd
008400970616     D CMDAPP          DS            50
008500970616     D  NOMMBR                42     48
008600970617     D CMDLOG          DS            80
008700970617     D  MBRLOG                42     48
008800970617     D  MBRFIS                71     77
008900970414     D WLBDA8          DS
009000970414     D  G02DAT                 1      8  0
009100970414     D  G02INV                 9     16  0
009200970414     D  G02ERR                17     17
009300970414     D  G02TGI                18     22  0
009400080421      *----------------------------------------------------------------
009500080421      * Controllo e semaforo x scrittura del VAC: TRUL47R
009600080421      *----------------------------------------------------------------
009700080421     D trul47ds      e ds
009800080421      *
009900970410     D KPJBA         E DS
010000970612     D DSPT          E DS                  EXTNAME(EDIDSPT)
010100970616     D DSPU          E DS                  EXTNAME(EDIDSPU)
010200980202     D DSEM          E DS                  EXTNAME(EDIDSEM)
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
012600971126     D FNLSI1        E DS                  EXTNAME(FNLSI1DS)
012700970410     D DS3K          E DS
012800970715     D DS3A          E DS
012900970701     D DS7J          E DS
013000970410     D DS7O          E DS
013100060811     D OG143         E DS
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
014300970410     I*--------------------------------------------------------------*
014400970410     I*  Definizione campi Azorg
014500970410     I*--------------------------------------------------------------*
014600970410     IAZORG01L  AA
014700990223     I                                  1    3  ORGDIT
014800990223     I                             P    4    5 0ORGFIL
014900060811     I                               3776 3800  ORGDE3
015000970410     C*--------------------------------------------------------------*
015100970410     C*  C I C L O     P R I N C I P A L E
015200970410     C*--------------------------------------------------------------*
015300970410     C*  Inizializzo varibili videata
015400970410     C                   EXSR      INZ01
015500080421      *
015600970410     C*  Gestione della prima videata
015700970410     C     WFINE         DOWEQ     'N'
015800970611     C     WTPVID        CASEQ     '1'           GESD01
015900970611     C     WTPVID        CASEQ     '2'           GESD02
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
017100970617     C                   MOVEL     'N'           WRIC81            1
017200970616     C                   MOVEL     'N'           WFINE
017300970611     C                   MOVEL     NOMPGM        V1CPGM
017400970611     C                   Z-ADD     0             V1CKSC
017500970611     C*
017600970611     C                   ENDSR
017700970410     C*--------------------------------------------------------------*
017800970611     C* INZ02: Impostazione della prima videata
017900970410     C*--------------------------------------------------------------*
018000970611     C     INZ02         BEGSR
018100970410     C*
018200970410     C                   SETOFF                                       284041
018300970611     C                   SETOFF                                       424344
018400970616     C                   MOVEL     '2'           WTPVID
018500970611     C                   MOVEL     NOMPGM        V2CPGM
018600970611     C                   MOVEL     V1CKSC        V2CKSC
018700970611     C                   MOVEL     ACORAG        V2DKSC
018800980204     C                   MOVEL     *BLANKS       V2DCOD
018900980204     C                   MOVEL     *BLANKS       V2CCOD
019000980202     C                   MOVEL     *BLANKS       V2CSTS
019100980202     C                   MOVEL     *BLANKS       V2CFTX
019200970611     C                   MOVE      WHHDAT        V2CAAS
019300130304      **  gestione cliente con Partner gestito da altra filiale
019400130304      **  vedi GWEISS con clienti sulla 277.
019500130304     c                   if        LNP_ALTRA_FIL = 0
019600971007     C                   Z-ADD     LNP(XY)       V2CLNP
019700130821      ******
019800130821     c**** Chiodo x Ducros da Eliminare fra 333 e 330
019900130821     c                   if        v2clnp =333
020000130821     c                   eval      v2clnp =330
020100130821     c                   end
020200130821      ******
020300130304     c                   else
020400130304     C                   eval        V2CLNP = LNP_ALTRA_FIL
020500130304     c                   end
020600971007     C                   Z-ADD     NRS(XY)       V2CNRS
020700970611     C                   Z-ADD     0             V2CNSP
020800970410     C*
020900970410     C                   ENDSR
021000970410     C*--------------------------------------------------------------*
021100970410     C* GESD01: Gestione della prima videata
021200970410     C*--------------------------------------------------------------*
021300970410     C     GESD01        BEGSR
021400970410     C*
021500970410     C* Eseguo exfmt della prima videata
021600970410     C                   EXFMT     LSI1D01
021700970611     C                   SETOFF                                       2840
021800970410     C                   SETOFF                                       4243
021900970410     C* Fine
022000970410     C     *INKC         IFEQ      '1'
022100970414     C                   MOVEL     'S'           WFINE             1
022200970410     C                   GOTO      FINVD1
022300970410     C                   END
022400970410     C* Controlli
022500970410     C                   EXSR      CTR01
022600970410     C   28              GOTO      FINVD1
022700970611     C*
022800970611     C* Se no errori passo a videata successiva
022900970611     C                   EXSR      INZ02
023000970410     C*
023100970410     C     FINVD1        ENDSR
023200970611     C*--------------------------------------------------------------*
023300970611     C* GESD02: Gestione della seconda videata
023400970611     C*--------------------------------------------------------------*
023500970611     C     GESD02        BEGSR
023600970611     C*
023700970611     C* Eseguo exfmt della prima videata
023800970611     C                   EXFMT     LSI1D02
023900970611     C                   SETOFF                                       284041
024000970611     C                   SETOFF                                       424344
024100970611     C* Fine
024200970611     C     *INKC         IFEQ      '1'
024300970611     C                   MOVEL     'S'           WFINE             1
024400970617     C* Controllo se devo richiamare TRTC81
024500970617     C     WRIC81        IFEQ      'S'
024600970617     C                   EXSR      CALL81
024700970617     C                   END
024800970611     C                   GOTO      FINVD2
024900970611     C                   END
025000970611     C* Prima videata
025100970611     C     *INKL         IFEQ      '1'
025200970611     C                   MOVEL     '1'           WTPVID            1
025300970617     C* Controllo se devo richiamare TRTC81
025400970617     C     WRIC81        IFEQ      'S'
025500970617     C                   EXSR      CALL81
025600970617     C                   END
025700970611     C                   GOTO      FINVD2
025800970611     C                   END
025900080421      *
026000970612     C* Interrogazione bolle
026100970612     C     *INKG         IFEQ      '1'
026200970616     C     V2CNSP        IFGT      0
026300970616     C                   CLEAR                   PARA05
026400970616     C                   Z-ADD     V2CAAS        P05AAS
026500970616     C                   Z-ADD     V2CLNP        P05LNP
026600970616     C                   Z-ADD     V2CNRS        P05NRS
026700970616     C                   Z-ADD     V2CNSP        P05NSP
026800970616     C                   MOVEL     '1'           P05GIA
026900970616     C                   MOVEL     PARA05        KPJBU
027000970616     C                   CALL      'FNLS05R'
027100970616     C                   PARM                    KPJBA
027200970616     C                   MOVEL     KPJBU         PARA05
027300980204     C     P05F03        IFEQ      *BLANKS
027400980204     C                   Z-ADD     P05AAS        V2CAAS
027500980204     C                   Z-ADD     P05LNP        V2CLNP
027600980204     C                   Z-ADD     P05NRS        V2CNRS
027700980204     C                   Z-ADD     P05NSP        V2CNSP
027800980204     C                   ENDIF
027900970616     C*
028000970616     C                   ELSE
028100970616     C*
028200970616     C                   CLEAR                   PARA04
028300970616     C                   MOVEL     '1'           P04FLG
028400970616     C                   MOVEL     PARA04        KPJBU
028500970616     C                   CALL      'FNLS04R'
028600970616     C                   PARM                    KPJBA
028700980204     C                   MOVEL     KPJBU         PARA04
028800980204     C     P04F03        IFEQ      *BLANKS
028900980204     C                   Z-ADD     P04AAS        V2CAAS
029000980204     C                   Z-ADD     P04LNP        V2CLNP
029100980204     C                   Z-ADD     P04NRS        V2CNRS
029200980204     C                   Z-ADD     P04NSP        V2CNSP
029300970616     C                   ENDIF
029400980204     C                   ENDIF
029500970612     C                   GOTO      FINVD2
029600970612     C                   END
029700970611     C* Controlli
029800970611     C                   EXSR      CTR02
029900970611     C   28              GOTO      FINVD2
030000970611     C*
030100970611     C* Se no errori vado a pgm successivo
030200970611     C                   EXSR      CALPGM
030300970616     C* Azzero prima videata se ho eseguito conferma
030400970616     C     DI1CMD        IFEQ      '06'
030500970617     C                   EXSR      INZ02
030600970616     C                   END
030700970611     C*
030800970611     C     FINVD2        ENDSR
030900970611     C*--------------------------------------------------------------*
031000970611     C* CTR01: Controlli prima videata
031100970611     C*--------------------------------------------------------------*
031200970611     C     CTR01         BEGSR
031300970611     C*
031400970611     C* Controllo se immesso codice cliente
031500970611     C     V1CKSC        IFEQ      0
031600970611     C                   SETON                                        2840
031700970611     C                   MOVEL     ERR(1)        $MSG
031800970611     C                   END
031900990610     C   28              GOTO      FINCT1
032000970611     C* Controllo se esiste cliente
032100970612     C                   Z-ADD     1             KKUT
032200970611     C                   Z-ADD     CLIITA        KKCC
032300970611     C                   Z-ADD     V1CKSC        KKSC
032400970611     C     KACO          CHAIN     CNACO00F                           31
032500970611     C     *IN31         IFEQ      '1'
032600970611     C                   SETON                                        2840
032700970611     C                   MOVEL     ERR(2)        $MSG
032800970611     C                   END
032900990610     C   28              GOTO      FINCT1
033000970617     C* Controllo Codice cliente unificante tabella 3K
033100030116     C                   Z-ADD     1             XX                5 0
033200970617     C     V1CKSC        LOOKUP    C3K(XX)                                32
033300060811      *
033400060811      **?se non ha agganciato nulla
033500060811     c                   if        *in32 = *off
033600060811     c     v1cksc        div       10000         trelnp            3 0
033700060811     C                   Z-ADD     treLNP        ORGFIL
033800060811     C     ORGFIL        CHAIN     AZORG01L                           31
033900060811      **? e se  si tratta di DPD (eccezione .......)
034000060811     c                   if        %Found(AZORG01L)
034100060811     c                   movel     orgde3        og143
034200060811     c                   if        §OGNTW ='DPD'
034300060811     C                   Z-ADD     1             XY                5 0
034400060811     C     V1CKSC        LOOKUP    KSC(XY)                                32
034500060811     c                   end
034600060811     c                   end
034700060811      **
034800060811     c                   else
034900060811      **
035000970617     C* Controllo se cliente in tabella PT
035100060811     C                   Z-ADD     1             XY                5 0
035200060811     C     CKS(XX)       LOOKUP    KSC(XY)                                32
035300060811     c                   end
035400060811      **
035500970611     C     *IN32         IFEQ      '0'
035600970611     C                   SETON                                        2840
035700970611     C                   MOVEL     ERR(3)        $MSG
035800970617     C                   ELSE
035900020912      *
036000020912      *  la Linea di Partenza del Partner deve essere gestita
036100020912      *   nella £1
036200020912     C     LNP(XY)       lookup    £1                                     32
036300080918      ** ----
036400130304      **     Potrebbe essere un cliente appartenente alla tabella CL
036500130304      **     gestito da un'altra filiale. a questo punto occorre controllare,
036600130304      **     non solo la LINEA del PARTNER agganciata sull'unificante della 3K
036700130304      **     ma se non fosse gestito il Partner sulla filiale si deve vedere
036800130304      **     la Linea del Cliente se è gestita sulla filiale.
036900130304      **   (Esempio su clienti di GWeiss della 360 dati in gestione alla 277.
037000130304      **    il cliente è il 2772159 unificato sulla 3K al 3600001.
037100130304      **   Non è gestita la 360 nella 277 allora il cliente 2772159 è della 277
037200130304      **    quindi deve passare il controllo...poichè il cliente è in gestione)
037300130304     c                   clear                   LNP_ALTRA_FIL     3 0
037400130304     c                   if        NOT *in32
037500130304     c     v1cksc        div       10000         treLNP            3 0
037600130304     C     treLNP        lookup    £1                                     32
037700130304     c   32              movel     treLNP        LNP_ALTRA_FIL
037800130304     c                   end
037900130304      ** ----
038000080918      **
038100080918      ** Occorre anche controllare se DPD di backup quindi linee collegate
038200080918      **  per potere gestire linee da filiali diverse
038300080918      **   ossia tab."3NN"
038400080918      **
038500080918     C     *IN32         ifeq      *off
038600080918     c                   movel(p)  '3NN'         TbeCOD
038700080918     c                   movel(p)  KSC(xy)       TbeKE1
038800080918     c     kTBE1         setll     tntbe01l
038900080918     c     kTBE1         reade     tntbe01l
039000080918     C                   dow       not %EoF(tntbe01l)
039100080918     c                   movel(p)  tbeKE2        LNPAlternativa    3 0
039200080918     C     LNPAlternativalookup    £1                                     32
039300080918     c                   if        *in32
039400080918     c                   z-add     1             z                 3 0
039500080918     c     *zero         lookup    £1(z)                                  21
039600080918      * aggiunge la linea x il controllo successivo
039700080918     c                   if        *in21
039800080918     C                   z-add     lnp(xy)       £1(z)
039900080918     C                   End
040000080918     C                   Leave
040100080918     C                   End
040200080918     c     kTBE1         reade     tntbe01l
040300080918     C                   End
040400080918     C                   End
040500080918      ** ----
040600020912     C     *IN32         ifeq      *off
040700020912     C                   SETON                                        2840
040800020912     C                   MOVEL     err(8)        $MSG
040900020912     C                   Else
041000020912     C*
041100020912     C                   EndIF
041200020912     C*
041300970617     C                   END
041400020912     C*
041500990610     C   28              GOTO      FINCT1
041600080421     C*
041700970611     C     FINCT1        ENDSR
041800970410     C*--------------------------------------------------------------*
041900130822     C* Decodifica i 2 riferimenti per impostare la spedizione
042000970410     C*--------------------------------------------------------------*
042100130822     C     decod_da_Rif  BEGSR
042200970410     C*
042300130822     c                   clear                   se_trovato        1
042400130822     c                   eval      se_trovato ='N'
042500130822     C*
042600130822     c                   if        v2rif32 <> *blank
042700130822     c     v2rif32       chain     fnblp32l
042800130822     c                   if        %Found(fnblp32l)
042900130822     c                   eval      se_trovato ='S'
043000130822     c                   end
043100130822     c                   end
043200130822      *
043300130822     c                   if        v2rif33 > 0 and
043400130822     c                             se_trovato ='N'
043500130822     c     v2rif33       chain     fnblp33l
043600130822     c                   if        %Found(fnblp33l)
043700130822     c                   eval      se_trovato ='S'
043800130822     c                   end
043900130822     c                   end
044000130822     C*
044100130822     C* Se lo ha trovato imposta imposta i campi della spedizione
044200130822     c                   IF        se_trovato ='S'
044300130822     c                   eval      v2caas = blpaas
044400130822     c                   eval      v2clnp = blplnp
044500130822     c                   eval      v2cnrs = blpnrs
044600130822     c                   eval      v2cnsp = blpnsp
044700130822     c                   end
044800130822     C*
044900130822     C                   ENDSR
045000130822     C*--------------------------------------------------------------*
045100130822     C* CTR02: Controlli prima videata
045200130822     C*--------------------------------------------------------------*
045300130822     C     CTR02         BEGSR
045400130822     C*
045500130822     C*  pre- controllo bolla tramite rif.alfa o numerico
045600130822     c                   exsr      decod_da_Rif
045700130822     C*
045800970410     C* Controllo se immesso anno
045900970611     C     V2CAAS        IFEQ      0
046000970410     C                   SETON                                        2840
046100970611     C                   MOVEL     ERR(5)        $MSG
046200970410     C                   END
046300990610     C   28              GOTO      FINCT2
046400970410     C* Se anno digitato di 2 --> 4 cifre
046500970612     C                   MOVEL     V2CAAS        WSECOL            2 0
046600970410     C     WSECOL        IFEQ      0
046700970611     C     V2CAAS        IFGT      60
046800970611     C                   ADD       1900          V2CAAS
046900970410     C                   ELSE
047000970611     C                   ADD       2000          V2CAAS
047100970410     C                   END
047200970410     C                   END
047300970410     C* Controllo se immessa linea di partenza
047400970611     C     V2CLNP        IFEQ      0
047500970414     C                   SETON                                        2841
047600970611     C                   MOVEL     ERR(6)        $MSG
047700970410     C                   END
047800990610     C   28              GOTO      FINCT2
047900970410     C* Controllo esistenza lnp
048000970611     C                   Z-ADD     V2CLNP        ORGFIL
048100970410     C     ORGFIL        CHAIN     AZORG01L                           31
048200970611     C     *IN31         IFEQ      '1'
048300970410     C                   SETON                                        2841
048400970611     C                   MOVEL     ERR(7)        $MSG
048500970414     C                   END
048600990610     C   28              GOTO      FINCT2
048700970414     C* Controllo se lnp in £1
048800970611     C     V2CLNP        LOOKUP    £1                                     32
048900970414     C     *IN32         IFEQ      '0'
049000970414     C                   SETON                                        2841
049100970611     C                   MOVEL     ERR(8)        $MSG
049200970414     C                   END
049300990610     C   28              GOTO      FINCT2
049400970414     C* Controllo se immesso numero di spedizione
049500970611     C     V2CNSP        IFEQ      0
049600970414     C                   SETON                                        2843
049700970611     C                   MOVEL     ERR(9)        $MSG
049800970414     C                   END
049900990610     C   28              GOTO      FINCT2
050000970414     C* Controllo se esiste spedizione
050100970611     C                   Z-ADD     V2CAAS        KAAS
050200970611     C                   Z-ADD     V2CLNP        KLNP
050300970611     C                   Z-ADD     V2CNRS        KNRS
050400970611     C                   Z-ADD     V2CNSP        KNSP
050500970414     C     KBLP          CHAIN(N)  FNBLP01L                           31
050600970414     C     *IN31         IFEQ      '1'
050700970414     C                   SETON                                        2843
050800970611     C                   MOVEL     ERR(10)       $MSG
050900970616     C                   ELSE
051000970616     C* Controllo che la spedizione sia del cliente selezionato
051100970616     C     BLPCCM        IFNE      V2CKSC
051200970616     C     BLPKSC        ANDNE     V2CKSC
051300970616     C                   SETON                                        2843
051400980202     C                   MOVEL     ERR(17)       $MSG
051500970414     C                   END
051600970616     C                   END
051700990610     C   28              GOTO      FINCT2
051800970414     C* Controllo se bolla mamma
051900970611     C                   Z-ADD     V2CAAS        KAAP
052000970611     C                   Z-ADD     V2CLNP        KLNP
052100970611     C                   Z-ADD     V2CNRS        KNRP
052200970611     C                   Z-ADD     V2CNSP        KNSP
052300970414     C     KBLP          CHAIN     FNLBL02L                           31
052400970414     C* Controllo se bolla figlia
052500970611     C                   Z-ADD     V2CAAS        KAAP
052600970611     C                   Z-ADD     V2CLNP        KLNP
052700970611     C                   Z-ADD     V2CNRS        KNRP
052800970611     C                   Z-ADD     V2CNSP        KNSP
052900970414     C     KBLP          CHAIN     FNLBL01L                           31
053000970414     C     *IN31         IFEQ      '0'
053100970611     C                   SETON                                        284344
053200970611     C                   MOVEL     ERR(12)       $MSG
053300970414     C                   END
053400990610     C   28              GOTO      FINCT2
053500980204     C* Controllo che non sia stata data per consegnata
053600980204     C     BLPDCM        IFNE      0
053700990225     C     BLPCCA        ANDNE     '6'
053800990225     C     BLPDCM        ORNE      0
053900990225     C     BLPCCA        ANDNE     '2'
054000980204     C                   SETON                                        2843
054100980204     C                   MOVEL     ERR(22)       $MSG
054200980204     C                   END
054300990610     C   28              GOTO      FINCT2
054400980204     C* Controllo che abbia il dettaglio segnacolli
054500980204     C                   Z-ADD     V2CAAS        KAAS
054600980204     C                   Z-ADD     V2CLNP        KLNP
054700980204     C                   Z-ADD     V2CNRS        KNRS
054800980204     C                   Z-ADD     V2CNSP        KNSP
054900980204     C     KBLT          CHAIN     FNBLT01L                           31
055000980204     C     *IN31         IFEQ      '1'
055100980204     C                   SETON                                        2843
055200980204     C                   MOVEL     ERR(16)       $MSG
055300980204     C                   END
055400990610     C   28              GOTO      FINCT2
055500980202     C* Controllo se richiesto '?'
055600980202     C     '?'           SCAN      V2CSTS                                 32
055700980202     C     *IN32         IFEQ      '1'
055800980202     C                   MOVE      LAB(XY)       DI1LAB
055900980202     C                   MOVEL     FNLSI1        KPJBU
056000980203     C                   CALL      'FNLSI1R5'
056100980202     C                   PARM                    KPJBA
056200980204     C                   MOVEL     KPJBU         FNLSI1
056300980204     C                   MOVEL     DI1CEV        V2CCOD
056400980202     C                   MOVEL     DI1STS        V2CSTS
056500980202     C                   END
056600980202     C* evento obbligatoria
056700980202     C     V2CCOD        IFEQ      *BLANKS
056800980202     C     V2CSTS        ANDEQ     *BLANKS
056900970721     C                   SETON                                        2844
057000980202     C                   MOVEL     ERR(21)       $MSG
057100970721     C                   END
057200990610     C   28              GOTO      FINCT2
057300980202     C* evento esistente
057400980202     C                   MOVEL     V2CSTS        WEVE              6
057500980202     C                   MOVE      V2CCOD        WEVE
057600030116     C                   Z-ADD     1             YY                5 0
057700980202     C     LAB(XY)       IFEQ      'S'
057800980202     C     WEVE          LOOKUP    ELB(YY)                                32
057900980202     C                   MOVEL     DLB(YY)       DSEM
058000980202     C                   ELSE
058100980202     C     WEVE          LOOKUP    EVG(YY)                                32
058200980202     C                   MOVEL     DVG(YY)       DSEM
058300980203     C                   MOVEL     §EMDES        V2DCOD
058400980202     C                   END
058500980204     C     *IN32         IFEQ      '0'
058600980202     C                   SETON                                        2844
058700980202     C                   MOVEL     ERR(19)       $MSG
058800980202     C                   END
058900990610     C   28              GOTO      FINCT2
059000980203     C* Se bolla in assegnato no IDD Totale definitivo
059100970715     C                   Z-ADD     1             X
059200970715     C     BLPCBO        LOOKUP    C3A(X)                                 32
059300970715     C                   MOVEL     TPO(X)        WTPO              1
059400990326     C     *IN32         IFEQ      '1'
059500990326     C     WTPO          ANDEQ     'A'
059600990326     C     §EMASS        ANDNE     'S'
059700990326     C                   SETON                                        2843
059800990326     C                   MOVEL     ERR(20)       $MSG
059900990326     C                   END
060000990610     C   28              GOTO      FINCT2
060100970414     C* Controllo che non sia stata data per partita
060200990609     C     BLPDT1        IFNE      0
060300990609     C     §EMTRA        ANDNE     'S'
060400990609     C                   SETON                                        2843
060500990609     C                   MOVEL     ERR(13)       $MSG
060600990609     C                   END
060700990610     C   28              GOTO      FINCT2
060800970414     C* Controllo che non sia stata trasmessa in sede
060900990609     C     BLPDT2        IFNE      0
061000990609     C     §EMTRA        ANDNE     'S'
061100990609     C                   SETON                                        2843
061200990609     C                   MOVEL     ERR(14)       $MSG
061300990609     C                   END
061400990610     C   28              GOTO      FINCT2
061500970414     C* Controllo che non sia stata trasmessa al cliente
061600990609     C     BLPDT3        IFNE      0
061700990609     C     §EMTRA        ANDNE     'S'
061800990609     C                   SETON                                        2843
061900990609     C                   MOVEL     ERR(15)       $MSG
062000990609     C                   END
062100990610     C   28              GOTO      FINCT2
062200990610      * Controllo C.A.
062300990610     C     §EMCAC        IFEQ      'S'
062400040715      *
062500040715     c                   clear                   fidn12ds
062600040715     c                   eval      i12aas = V2CAAS
062700040715     c                   eval      i12lnp = V2CLNP
062800040715     c                   eval      i12nrs = V2CNRS
062900040715     c                   eval      i12nsp = V2CNSP
063000040715     c                   eval      i12fel = 'E'
063100040715     c                   eval      i12fan = 'E'
063200040715     c                   eval      i12fch = 'E'
063300040715     c                   call      'FIDN12R'
063400040715     c                   parm                    fidn12ds
063500040715      **
063600040715      * se c'è almeno una C.A. aperta
063700040715     c     o12nca        ifgt      0
063800040715     C                   SETON                                        284331
063900040715     C                   MOVEL     ERR(24)       $MSG
064000040715     c                   end
064100040715      **
064200990610     C                   ENDIF
064300040715      **
064400990610     C   28              GOTO      FINCT2
064500040715      **
064600980204     C* Controllo se l'evento è già stato inviato
064700980204     C                   Z-ADD     V2CAAS        KAA2
064800980204     C                   MOVEL     §EMCOD        KCEV
064900980204     C                   MOVEL     ' '           WDAFOR            1
065000990810     C     KEVB          CHAIN     FNEVB04L                           31
065100980204     C* Evento già immesso
065200980204     C     *IN31         IFEQ      '0'
065300980204     C* se non è possibile forzare do errore
065400980204     C     §EMFOR        IFNE      'S'
065500980204     C                   SETON                                        2843
065600980204     C                   MOVEL     ERR(23)       $MSG
065700980204     C                   ELSE
065800980204     C                   MOVEL     'F'           WDAFOR
065900980204     C                   END
066000980204     C                   END
066100990610     C   28              GOTO      FINCT2
066200970414     C*
066300970612     C     FINCT2        ENDSR
066400970612     C*--------------------------------------------------------------*
066500970612     C* CALPGM: Eseguo Call a pgm
066600970612     C*--------------------------------------------------------------*
066700970612     C     CALPGM        BEGSR
066800970612     C*
066900970612     C*  Imposto DS per richiamo altri pgm
067000970612     C                   CLEAR                   FNLSI1
067100980203     C                   MOVEL     V2CFTX        DI1FTX
067200980203     C                   MOVEL     V2CKSC        DI1KSC
067300970612     C                   MOVEL     V2DKSC        DI1DEC
067400970612     C                   MOVEL     V2CLNP        DI1LNP
067500970612     C                   MOVEL     V2CNRS        DI1NRS
067600970612     C                   MOVEL     V2CNSP        DI1NSP
067700970612     C                   MOVEL     V2CAAS        DI1AAS
067800970612     C                   MOVEL     V2CAAS        DI1DSP
067900970612     C                   MOVE      BLPMGS        DI1DSP
068000970616     C                   MOVE      BLPMGS        DI1DSP
068100971007     C                   MOVE      LAB(XY)       DI1LAB
068200980202     C                   MOVE      V2CSTS        DI1STS
068300980202     C                   MOVE      V2CCOD        DI1CEV
068400980202     C                   MOVE      §EMDES        DI1DEV
068500980203     C                   MOVE      §EMDES        DI1DEV
068600980203     C                   MOVE      §EMCOD        DI1C2A
068700980203     C                   MOVE      §EMCCA        DI1CCA
068800980203     C                   MOVE      §EMBLP        DI1BLP
068900980203     C                   MOVE      §EMBLT        DI1BLT
069000980203     C                   MOVE      §EMFOR        DI1FRZ
069100980203     C                   MOVE      §EMTOT        DI1TOT
069200980204     C                   MOVE      WDAFOR        DI1FOR
069300980204     C                   MOVE      BLPNCL        DI1NCL
069400970612     C                   MOVEL     FNLSI1        KPJBU
069500980202     C* Richiamo pgm gestione dettaglio segnacolli
069600980202     C     §EMTOT        IFNE      'S'
069700980202     C                   CALL      'FNLSI1R2'
069800980202     C                   PARM                    KPJBA
069900980202     C                   MOVEL     KPJBU         FNLSI1
070000980202     C                   END
070100980202     C* Se ho digitato F3 esco
070200980202     C     DI1CMD        IFEQ      '03'
070300980202     C                   MOVEL     'S'           WFINE
070400980202     C                   GOTO      FINRIC
070500980202     C                   END
070600980204     C* Se ho digitato F12 azzero campo scelta
070700980204     C     DI1CMD        IFEQ      '12'
070800980204     C                   GOTO      FINRIC
070900980204     C                   END
071000980202     C* Richiamo pgm gestione terza videata: SPEDIZIONE
071100970612     C                   CALL      'FNLSI1R1'
071200970612     C                   PARM                    KPJBA
071300970617     C                   MOVEL     KPJBU         FNLSI1
071400970612     C*
071500970612     C* Se ho digitato F3 esco
071600970612     C                   SELECT
071700970612     C     DI1CMD        WHENEQ    '03'
071800970619     C                   MOVEL     'S'           WFINE
071900970619     C* Controllo se devo richiamare TRTC81
072000970619     C     WRIC81        IFEQ      'S'
072100970619     C                   EXSR      CALL81
072200970619     C                   END
072300970612     C* Se ho digitato F12 azzero campo scelta
072400970612     C     DI1CMD        WHENEQ    '12'
072500970616     C* CONFERMA
072600970616     C     DI1CMD        WHENEQ    '06'
072700970617     C                   MOVEL     'S'           WRIC81
072800970616     C* Aggiorno BLP/BLT
072900980202     C     §EMBLP        IFEQ      'S'
073000970612     C                   CALL      'FNLSI1R3'
073100970617     C                   PARM                    KPJBA
073200970616     C                   END
073300080421      * l'EDIVAC viene sostituito con il TIVGD Download standard
073400080421     C* Scrivo EDVAC --> TIVGD00F
073500970612     C                   CALL      'FNLSI1R4'
073600970612     C                   PARM                    KPJBA
073700970616     C                   ENDSL
073800970612     C*
073900980202     C     FINRIC        ENDSR
074000970617     C*--------------------------------------------------------------*
074100980126     C* CALL81: PER E.D.I. RICHIAMO PGM TRTC81C
074200970617     C*--------------------------------------------------------------*
074300970617     C     CALL81        BEGSR
074400970617     C*
074500980126      * CALL A TRTC81C PER E.D.I.
074600980126     C     §PTFTT        IFNE      'S'
074700061124     C     §PTFTT        ANDNE     'P'
074800980126     C                   MOVEL     'S'           WCAL81            1
074900080421      *
075000080421      *  Scrive IFTSTA x EDI:
075100080421      * ? il TRTC81C  attiva giro di trasm con SNDIFTSTA  ?
075200080421     C                   MOVEL     *all'0'       Elab_Cliente
075300080421     C                   MOVE      CKS(XX)       Elab_Cliente
075400080421     c                   eval      Tipo_Scarico = CSD(XX)
075500080421      *
075600080421     c                   eval      Tipo_File = 'VC'
075700110630     C                   CALL      'TRTCT81C'
075800080421     c                   parm                    Elab_Cliente      8
075900080422     c                   parm                    Tipo_File         2
076000080421     c                   parm                    Tipo_Scarico      2
076100980126      *
076200980126     C                   END
076300980126      *
076400970617     C                   ENDSR
076500970410     C*--------------------------------------------------------------*
076600970410     C* *INZSR: Operazioni iniziali
076700970410     C*--------------------------------------------------------------*
076800970410     C     *INZSR        BEGSR
076900970410     C*
077000970410     C     *ENTRY        PLIST
077100970410     C                   PARM                    KPJBA
077200990223     C* Richiamo xparut
077300970410     C                   Z-ADD     1             CODUT
077400970410     C                   CALL      'X§PARUT'
077500970410     C                   PARM                    UTEDSE
077600970410     C                   MOVEL     REC80         CNCR80
077700970410     C                   MOVEL     RAGUT         RSUT
077800030924      *
077900030924      *  In base all'utente prende il P.O.
078000030924     C                   CLEAR                   Tibs36Ds
078100030924     c                   EVAL      I36ute = Knmus
078200030924     c                   EVAL      I36Tla = 'L'
078300030924     C                   CALL      'TIBS36R'
078400030924     C                   PARM                    Tibs36Ds
078500030924      *
078600970612     C*--- RICERCA CAPOCONTI------------------------------------------*
078700030116     C                   DO        50            X                 5 0
078800970612     C                   MOVE      TCU(X)        TCUDS
078900970612     C     F56           IFEQ      'CG'
079000970612     C     F4            ANDEQ     '1'
079100970612     C     F3            ANDEQ     '0'
079200970612     C                   Z-ADD     KCU(X)        CLIITA            4 0
079300970612     C                   END
079400970612     C                   END
079500970612     C*--- RICERCA CAPOCONTI------------------------------------------*
079600970410      *  Definizione chiavi
079700970410     C     KBLP          KLIST
079800970410     C                   KFLD                    KAAS
079900970410     C                   KFLD                    KLNP
080000970410     C                   KFLD                    KNRS
080100970410     C                   KFLD                    KNSP
080200970410     C     KBLT          KLIST
080300970410     C                   KFLD                    KAAS
080400970410     C                   KFLD                    KLNP
080500970410     C                   KFLD                    KNRS
080600970410     C                   KFLD                    KNSP
080700970410     C     KLBL1         KLIST
080800970410     C                   KFLD                    KAAP
080900970410     C                   KFLD                    KLPP
081000970410     C                   KFLD                    KNRP
081100970410     C                   KFLD                    KNSP
081200970410     C     KLBL2         KLIST
081300970410     C                   KFLD                    KAAN
081400970410     C                   KFLD                    KLPN
081500970410     C                   KFLD                    KNRN
081600970410     C                   KFLD                    KNSN
081700970410     C     KTAB1         KLIST
081800970410     C                   KFLD                    KKUT
081900970410     C                   KFLD                    KCOD
082000970410     C     KTAB2         KLIST
082100970410     C                   KFLD                    KKUT
082200970410     C                   KFLD                    KCOD
082300970410     C                   KFLD                    KKEY
082400970612     C     KACO          KLIST
082500970612     C                   KFLD                    KKUT
082600970612     C                   KFLD                    KKCC
082700970612     C                   KFLD                    KKSC
082800980204     C     KEVB          KLIST
082900980204     C                   KFLD                    KAA2
083000980204     C                   KFLD                    KLNP
083100980204     C                   KFLD                    KNRS
083200980204     C                   KFLD                    KNSP
083300980204     C                   KFLD                    KCEV
083400080918     C     KTBE1         KLIST
083500080918     C                   KFLD                    TBECOD
083600080918     C                   KFLD                    TBEKE1
083700970410      *  Definizione variabili
083800980204     C     *LIKE         DEFINE    EVBAAS        KAA2
083900980204     C     *LIKE         DEFINE    EVBCEV        KCEV
084000970410     C     *LIKE         DEFINE    BLPAAS        KAAS
084100970410     C     *LIKE         DEFINE    BLPLNP        KLNP
084200970410     C     *LIKE         DEFINE    BLPNRS        KNRS
084300970410     C     *LIKE         DEFINE    BLPNSP        KNSP
084400970410     C     *LIKE         DEFINE    LBLAAP        KAAP
084500970410     C     *LIKE         DEFINE    LBLLPP        KLPP
084600970410     C     *LIKE         DEFINE    LBLNRP        KNRP
084700970410     C     *LIKE         DEFINE    LBLAAN        KAAN
084800970410     C     *LIKE         DEFINE    LBLLPN        KLPN
084900970410     C     *LIKE         DEFINE    LBLNRN        KNRN
085000970410     C     *LIKE         DEFINE    LBLNSN        KNSN
085100970410     C     *LIKE         DEFINE    TBLKUT        KKUT
085200970410     C     *LIKE         DEFINE    TBLCOD        KCOD
085300970410     C     *LIKE         DEFINE    TBLKEY        KKEY
085400970611     C     *LIKE         DEFINE    TABCOD        KCODE
085500970612     C     *LIKE         DEFINE    ACOKCC        KKCC
085600970612     C     *LIKE         DEFINE    ACOKSC        KKSC
085700970410      *
085800970410      *  Reperisco la data del giorno
085900970410     C                   TIME                    WHHDAT           14 0
086000970414     C                   MOVE      WHHDAT        G02DAT
086100970414     C                   MOVEL     *BLANK        G02ERR
086200970414     C                   CALL      'XSRDA8'
086300970414     C                   PARM                    WLBDA8
086400970414     C                   Z-ADD     G02INV        OGGI              8 0
086500970611     C*
086600970611     C* Carico dati DS partner in schiera
086700970612     C                   OPEN      EDTAB01L
086800970611     C                   Z-ADD     1             X
086900970611     C                   MOVEL     'PT'          KCODE
087000970616     C     KCODE         CHAIN     EDTAB01L                           30
087100970611     C     *IN30         DOWEQ     '0'
087200970611     C     TABFLG        IFEQ      ' '
087300970611     C                   ADD       1             X
087400970611     C                   MOVEL     TABUNI        DSPT
087500970611     C                   MOVEL     §PTKSC        KSC(X)
087600970616     C                   MOVEL     §PTLNP        LNP(X)
087700970616     C                   MOVEL     §PTNRS        NRS(X)
087800970611     C                   END
087900970616     C     KCODE         READE     EDTAB01L                               30
088000970611     C                   END
088100970616     C*
088200970616     C                   Z-ADD     1             X
088300970616     C                   MOVEL     'PU'          KCODE
088400970616     C     KCODE         CHAIN     EDTAB01L                           30
088500970616     C     *IN30         DOWEQ     '0'
088600970616     C     TABFLG        IFEQ      ' '
088700970616     C                   ADD       1             X
088800970616     C                   MOVEL     TABUNI        DSPU
088900970616     C                   MOVEL     §PULAB        LAB(X)
089000970617     C                   MOVEL     §PUTPC        TPC(X)
089100970616     C                   END
089200970616     C     KCODE         READE     EDTAB01L                               30
089300970616     C                   END
089400980202     C*
089500980202     C* Carico in schiera tabella eventi manuali
089600980202     C                   Z-ADD     0             X
089700030116     C                   Z-ADD     0             X1                5 0
089800980202     C                   MOVEL     'EM'          KCODE
089900980202     C     KCODE         CHAIN     EDTAB01L                           30
090000980202     C     *IN30         DOWEQ     '0'
090100980202     C     TABFLG        IFEQ      ' '
090200980202     C                   MOVEL     TABUNI        DSEM
090300980202     C     §EMLAB        IFEQ      'S'
090400980202     C                   ADD       1             X
090500980204     C                   MOVEL     TABKEY        ELB(X)
090600980202     C                   MOVEL     TABUNI        DLB(X)
090700980202     C                   END
090800980202     C     §EMVGL        IFEQ      'S'
090900980202     C                   ADD       1             X1
091000980204     C                   MOVEL     TABKEY        EVG(X1)
091100980202     C                   MOVEL     TABUNI        DVG(X1)
091200980202     C                   END
091300980202     C     KCODE         READE     EDTAB01L                               30
091400980202     C                   END
091500980202     C                   END
091600970611     C* close EDTAB
091700970611     C                   CLOSE     EDTAB01L
091800970611     C*
091900970410     C* Eseguo caricamento tabella 3K
092000970612     C                   OPEN      TABEL00F
092100970410     C                   Z-ADD     0             X
092200970410     C                   Z-ADD     1             KKUT
092300970410     C                   MOVEL     '3K'          KCOD
092400970410     C     KTAB1         CHAIN     TABEL                              30
092500970410     C     *IN30         DOWEQ     '0'
092600030213     C     X             ANDLT     9990
092700970410     C     TBLFLG        IFEQ      *BLANKS
092800970410     C                   ADD       1             X
092900970617     C                   MOVEL     TBLUNI        DS3K
093000970617     C                   MOVEL     TBLKEY        C3K(X)
093100970617     C                   MOVEL     §3KCKS        CKS(X)
093200010725     C                   MOVEL     §3KCSD        CSD(X)
093300970410     C                   END
093400970410     C     KTAB1         READE     TABEL                                  30
093500970410     C                   END
093600970715     C* Eseguo caricamento tabella 3A
093700970721     C                   Z-ADD     0             X
093800970715     C                   MOVEL     '3A'          KCOD
093900970715     C     KTAB1         CHAIN     TABEL                              30
094000970715     C     *IN30         DOWEQ     '0'
094100970721     C     X             ANDLT     500
094200970715     C     TBLFLG        IFEQ      *BLANKS
094300970715     C                   ADD       1             X
094400970715     C                   MOVEL     TBLUNI        DS3A
094500970715     C                   MOVEL     TBLKEY        C3A(X)
094600970715     C                   MOVEL     §3ATB1        TPO(X)
094700970715     C                   END
094800970715     C     KTAB1         READE     TABEL                                  30
094900970715     C                   END
095000970701     C*
095100970701     C* Eseguo caricamento da tabella 7J codici pistole fittizie
095200970701     C                   MOVEA     *HIVAL        C7J
095300970701     C                   Z-ADD     0             X
095400970701     C                   Z-ADD     1             KKUT
095500970701     C                   MOVEL     '7J'          KCOD
095600970701     C     KTAB1         CHAIN     TABEL                              30
095700970701     C     *IN30         DOWEQ     '0'
095800970721     C     X             ANDLT     500
095900970701     C     TBLFLG        IFEQ      *BLANKS
096000970701     C                   MOVEL     TBLUNI        DS7J
096100970912     C     §7JRPS        IFEQ      'A'
096200970701     C                   ADD       1             X
096300970701     C                   MOVEL     TBLKEY        C7J(X)
096400970701     C                   END
096500970701     C                   END
096600970701     C     KTAB1         READE     TABEL                                  30
096700970701     C                   END
096800970410     C* Eseguo caricamento £1
096900020506     c                   clear                   trul06ds
097000020506     c                   eval      d06cod = '£1'
097100030924     c                   movel     o36pou        d06key
097200020506     c                   movel     trul06ds      kpjbu
097300020506     c                   call      'TRUL06R'
097400020506     c                   parm                    kpjba
097500020506     c                   movel     kpjbu         trul06ds
097600020506     c                   movea     lin           £1
097700020912      *
097800971008     C* Imposto a 'N' flag richiamo TRTC81C
097900971008     C                   MOVEL     'N'           WCAL81            1
098000970410     C* Eseguo close del file
098100970410     C                   CLOSE     TABEL00F
098200970410     C*
098300970410     C                   ENDSR
098400080930      *------------------------------------------------------------------------*
098500080930      * Routine di controllo x eventuali Errori
098600080930      *------------------------------------------------------------------------*
098700080930     C     *PSSR         BEGSR
098800080930      *
098900080930      * Comunque eseguo x rilasciare il TIVGD.
099000080930      *? Infine elimino il blocco elaborazione TIVGD x tipo file qui.  ?
099100080930     C                   clear                   trul47ds
099200080930     C                   eval      d47opz  = 'F'
099300080930     C                   eval      d47tip  = 'VC'
099400080930     C                   call      'TRUL47R'
099500080930     C                   parm                    trul47ds
099600080930      *
099700080930     C                   ENDSR     '*CANCL'
099800080930     C*------------------------------------------------------------------------*
099900971008**
100000970701Immettere codice cliente                                                      01
100100970701Codice cliente inesistente                                                    02
100200060811Codice cliente non presente in tabella 3K o PT                                03
100300970701Un altro utente sta chiudendo le spedizioni del cliente                       04
100400970701Immettere anno della spedizione                                               05
100500970701Immettere linea di partenza della spedizione                                  06
100600970701Linea di partenza inesistente                                                 07
100700970701Linea di partenza non gestita su questo as                                    08
100800970701Immettere numero della spedizione                                             09
100900970701Spedizione inesistente                                                        10
101000980206Bolla mamma: impossibile gestirla                                             11
101100980206Bolla figlia: impossibile gestirla                                            12
101200970701Spedizione già trasmessa alla filiale di arrivo                               13
101300970701Spedizione già trasmessa alla sede                                            14
101400970701Spedizione già trasmessa al cliente                                           15
101500980202Spedizione senza colli: impossibile eseguire chiusura                         16
101600980202Spedizione non appartenente al cliente selezionato                            17
101700980202Il codice selezionato deve appartenere ad un partner                          18
101800980202Codice evento inesistente                                                     19
101900980202Bolla in assegnato: chiusura definitiva possibile solo in arrivo              20
102000980202Codice evento obbligatorio                                                    21
102100980204Spedizione già consegnata                                                     22
102200980204Evento già inviato impossibile rinviarlo                                      23
102300990610Esistono C.A. aperte per la spedizione, codice evento non valido              24
