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
006500131016     D ERR             S             78    DIM(26) CTDATA PERRCD(1)
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
042300131016     c                   clear                   piu_bolle_Xrif    1
042400131016     c                   clear                   V2NOTE1
042500131016     c                   clear                   V2NOTE2
042600131016     C*
042700130822     c                   clear                   se_trovato        1
042800130822     c                   eval      se_trovato ='N'
042900130822     C*
043000130822     c                   if        v2rif32 <> *blank
043100131016     c     v2rif32       setll     fnblp32l
043200131016     c     v2rif32       reade     fnblp32l
043300131016     c                   dow       not %EoF(fnblp32l)
043400130822     c                   eval      se_trovato ='S'
043500131016     c     v2rif32       reade     fnblp32l
043600131016     c                   if        not %EoF(fnblp32l)
043700131016     c                   eval       piu_bolle_Xrif ='S'
043800131016     c                   end
043900130822     c                   end
044000130822     c                   end
044100130822      *
044200130822     c                   if        v2rif33 > 0 and
044300130822     c                             se_trovato ='N'
044400131016     c     v2rif33       setll     fnblp33l
044500131016     c     v2rif33       reade     fnblp33l
044600131016     c                   dow       not %Eof(fnblp33l)
044700130822     c                   eval      se_trovato ='S'
044800131016     c     v2rif33       reade     fnblp33l
044900131016     c                   if        not %EoF(fnblp33l)
045000131016     c                   eval       piu_bolle_Xrif ='S'
045100131016     c                   end
045200130822     c                   end
045300130822     c                   end
045400130822     C*
045500130822     C* Se lo ha trovato imposta imposta i campi della spedizione
045600131016     C*   ATTENZIONE potrebbero essercene più di una con lo stesso
045700131016     C* riferimento quindi segnala che ha preso l'ultima
045800130822     c                   IF        se_trovato ='S'
045900131016     c                               and v2cnsp = 0
046000130822     c                   eval      v2caas = blpaas
046100130822     c                   eval      v2clnp = blplnp
046200130822     c                   eval      v2cnrs = blpnrs
046300130822     c                   eval      v2cnsp = blpnsp
046400130822     c                   end
046500131016     C*   AVVISA SE HA trovato più bolle con lo stesso riferimento
046600131016     c                   IF         piu_bolle_Xrif ='S'
046700131016     c                   EVAL        V2NOTE1 = err(25)
046800131016     c                   EVAL        V2NOTE2 = err(26)
046900131016     c                   end
047000131016     C*
047100130822     C                   ENDSR
047200130822     C*--------------------------------------------------------------*
047300130822     C* CTR02: Controlli prima videata
047400130822     C*--------------------------------------------------------------*
047500130822     C     CTR02         BEGSR
047600130822     C*
047700130822     C*  pre- controllo bolla tramite rif.alfa o numerico
047800130822     c                   exsr      decod_da_Rif
047900130822     C*
048000970410     C* Controllo se immesso anno
048100970611     C     V2CAAS        IFEQ      0
048200970410     C                   SETON                                        2840
048300970611     C                   MOVEL     ERR(5)        $MSG
048400970410     C                   END
048500990610     C   28              GOTO      FINCT2
048600970410     C* Se anno digitato di 2 --> 4 cifre
048700970612     C                   MOVEL     V2CAAS        WSECOL            2 0
048800970410     C     WSECOL        IFEQ      0
048900970611     C     V2CAAS        IFGT      60
049000970611     C                   ADD       1900          V2CAAS
049100970410     C                   ELSE
049200970611     C                   ADD       2000          V2CAAS
049300970410     C                   END
049400970410     C                   END
049500970410     C* Controllo se immessa linea di partenza
049600970611     C     V2CLNP        IFEQ      0
049700970414     C                   SETON                                        2841
049800970611     C                   MOVEL     ERR(6)        $MSG
049900970410     C                   END
050000990610     C   28              GOTO      FINCT2
050100970410     C* Controllo esistenza lnp
050200970611     C                   Z-ADD     V2CLNP        ORGFIL
050300970410     C     ORGFIL        CHAIN     AZORG01L                           31
050400970611     C     *IN31         IFEQ      '1'
050500970410     C                   SETON                                        2841
050600970611     C                   MOVEL     ERR(7)        $MSG
050700970414     C                   END
050800990610     C   28              GOTO      FINCT2
050900970414     C* Controllo se lnp in £1
051000970611     C     V2CLNP        LOOKUP    £1                                     32
051100970414     C     *IN32         IFEQ      '0'
051200970414     C                   SETON                                        2841
051300970611     C                   MOVEL     ERR(8)        $MSG
051400970414     C                   END
051500990610     C   28              GOTO      FINCT2
051600970414     C* Controllo se immesso numero di spedizione
051700970611     C     V2CNSP        IFEQ      0
051800970414     C                   SETON                                        2843
051900970611     C                   MOVEL     ERR(9)        $MSG
052000970414     C                   END
052100990610     C   28              GOTO      FINCT2
052200970414     C* Controllo se esiste spedizione
052300970611     C                   Z-ADD     V2CAAS        KAAS
052400970611     C                   Z-ADD     V2CLNP        KLNP
052500970611     C                   Z-ADD     V2CNRS        KNRS
052600970611     C                   Z-ADD     V2CNSP        KNSP
052700970414     C     KBLP          CHAIN(N)  FNBLP01L                           31
052800970414     C     *IN31         IFEQ      '1'
052900970414     C                   SETON                                        2843
053000970611     C                   MOVEL     ERR(10)       $MSG
053100970616     C                   ELSE
053200970616     C* Controllo che la spedizione sia del cliente selezionato
053300970616     C     BLPCCM        IFNE      V2CKSC
053400970616     C     BLPKSC        ANDNE     V2CKSC
053500970616     C                   SETON                                        2843
053600980202     C                   MOVEL     ERR(17)       $MSG
053700970414     C                   END
053800970616     C                   END
053900990610     C   28              GOTO      FINCT2
054000970414     C* Controllo se bolla mamma
054100970611     C                   Z-ADD     V2CAAS        KAAP
054200970611     C                   Z-ADD     V2CLNP        KLNP
054300970611     C                   Z-ADD     V2CNRS        KNRP
054400970611     C                   Z-ADD     V2CNSP        KNSP
054500970414     C     KBLP          CHAIN     FNLBL02L                           31
054600970414     C* Controllo se bolla figlia
054700970611     C                   Z-ADD     V2CAAS        KAAP
054800970611     C                   Z-ADD     V2CLNP        KLNP
054900970611     C                   Z-ADD     V2CNRS        KNRP
055000970611     C                   Z-ADD     V2CNSP        KNSP
055100970414     C     KBLP          CHAIN     FNLBL01L                           31
055200970414     C     *IN31         IFEQ      '0'
055300970611     C                   SETON                                        284344
055400970611     C                   MOVEL     ERR(12)       $MSG
055500970414     C                   END
055600990610     C   28              GOTO      FINCT2
055700980204     C* Controllo che non sia stata data per consegnata
055800980204     C     BLPDCM        IFNE      0
055900990225     C     BLPCCA        ANDNE     '6'
056000990225     C     BLPDCM        ORNE      0
056100990225     C     BLPCCA        ANDNE     '2'
056200980204     C                   SETON                                        2843
056300980204     C                   MOVEL     ERR(22)       $MSG
056400980204     C                   END
056500990610     C   28              GOTO      FINCT2
056600980204     C* Controllo che abbia il dettaglio segnacolli
056700980204     C                   Z-ADD     V2CAAS        KAAS
056800980204     C                   Z-ADD     V2CLNP        KLNP
056900980204     C                   Z-ADD     V2CNRS        KNRS
057000980204     C                   Z-ADD     V2CNSP        KNSP
057100980204     C     KBLT          CHAIN     FNBLT01L                           31
057200980204     C     *IN31         IFEQ      '1'
057300980204     C                   SETON                                        2843
057400980204     C                   MOVEL     ERR(16)       $MSG
057500980204     C                   END
057600990610     C   28              GOTO      FINCT2
057700980202     C* Controllo se richiesto '?'
057800980202     C     '?'           SCAN      V2CSTS                                 32
057900980202     C     *IN32         IFEQ      '1'
058000980202     C                   MOVE      LAB(XY)       DI1LAB
058100980202     C                   MOVEL     FNLSI1        KPJBU
058200980203     C                   CALL      'FNLSI1R5'
058300980202     C                   PARM                    KPJBA
058400980204     C                   MOVEL     KPJBU         FNLSI1
058500980204     C                   MOVEL     DI1CEV        V2CCOD
058600980202     C                   MOVEL     DI1STS        V2CSTS
058700980202     C                   END
058800980202     C* evento obbligatoria
058900980202     C     V2CCOD        IFEQ      *BLANKS
059000980202     C     V2CSTS        ANDEQ     *BLANKS
059100970721     C                   SETON                                        2844
059200980202     C                   MOVEL     ERR(21)       $MSG
059300970721     C                   END
059400990610     C   28              GOTO      FINCT2
059500980202     C* evento esistente
059600980202     C                   MOVEL     V2CSTS        WEVE              6
059700980202     C                   MOVE      V2CCOD        WEVE
059800030116     C                   Z-ADD     1             YY                5 0
059900980202     C     LAB(XY)       IFEQ      'S'
060000980202     C     WEVE          LOOKUP    ELB(YY)                                32
060100980202     C                   MOVEL     DLB(YY)       DSEM
060200980202     C                   ELSE
060300980202     C     WEVE          LOOKUP    EVG(YY)                                32
060400980202     C                   MOVEL     DVG(YY)       DSEM
060500980203     C                   MOVEL     §EMDES        V2DCOD
060600980202     C                   END
060700980204     C     *IN32         IFEQ      '0'
060800980202     C                   SETON                                        2844
060900980202     C                   MOVEL     ERR(19)       $MSG
061000980202     C                   END
061100990610     C   28              GOTO      FINCT2
061200980203     C* Se bolla in assegnato no IDD Totale definitivo
061300970715     C                   Z-ADD     1             X
061400970715     C     BLPCBO        LOOKUP    C3A(X)                                 32
061500970715     C                   MOVEL     TPO(X)        WTPO              1
061600990326     C     *IN32         IFEQ      '1'
061700990326     C     WTPO          ANDEQ     'A'
061800990326     C     §EMASS        ANDNE     'S'
061900990326     C                   SETON                                        2843
062000990326     C                   MOVEL     ERR(20)       $MSG
062100990326     C                   END
062200990610     C   28              GOTO      FINCT2
062300970414     C* Controllo che non sia stata data per partita
062400990609     C     BLPDT1        IFNE      0
062500990609     C     §EMTRA        ANDNE     'S'
062600990609     C                   SETON                                        2843
062700990609     C                   MOVEL     ERR(13)       $MSG
062800990609     C                   END
062900990610     C   28              GOTO      FINCT2
063000970414     C* Controllo che non sia stata trasmessa in sede
063100990609     C     BLPDT2        IFNE      0
063200990609     C     §EMTRA        ANDNE     'S'
063300990609     C                   SETON                                        2843
063400990609     C                   MOVEL     ERR(14)       $MSG
063500990609     C                   END
063600990610     C   28              GOTO      FINCT2
063700970414     C* Controllo che non sia stata trasmessa al cliente
063800990609     C     BLPDT3        IFNE      0
063900990609     C     §EMTRA        ANDNE     'S'
064000990609     C                   SETON                                        2843
064100990609     C                   MOVEL     ERR(15)       $MSG
064200990609     C                   END
064300990610     C   28              GOTO      FINCT2
064400990610      * Controllo C.A.
064500990610     C     §EMCAC        IFEQ      'S'
064600040715      *
064700040715     c                   clear                   fidn12ds
064800040715     c                   eval      i12aas = V2CAAS
064900040715     c                   eval      i12lnp = V2CLNP
065000040715     c                   eval      i12nrs = V2CNRS
065100040715     c                   eval      i12nsp = V2CNSP
065200040715     c                   eval      i12fel = 'E'
065300040715     c                   eval      i12fan = 'E'
065400040715     c                   eval      i12fch = 'E'
065500040715     c                   call      'FIDN12R'
065600040715     c                   parm                    fidn12ds
065700040715      **
065800040715      * se c'è almeno una C.A. aperta
065900040715     c     o12nca        ifgt      0
066000040715     C                   SETON                                        284331
066100040715     C                   MOVEL     ERR(24)       $MSG
066200040715     c                   end
066300040715      **
066400990610     C                   ENDIF
066500040715      **
066600990610     C   28              GOTO      FINCT2
066700040715      **
066800980204     C* Controllo se l'evento è già stato inviato
066900980204     C                   Z-ADD     V2CAAS        KAA2
067000980204     C                   MOVEL     §EMCOD        KCEV
067100980204     C                   MOVEL     ' '           WDAFOR            1
067200990810     C     KEVB          CHAIN     FNEVB04L                           31
067300980204     C* Evento già immesso
067400980204     C     *IN31         IFEQ      '0'
067500980204     C* se non è possibile forzare do errore
067600980204     C     §EMFOR        IFNE      'S'
067700980204     C                   SETON                                        2843
067800980204     C                   MOVEL     ERR(23)       $MSG
067900980204     C                   ELSE
068000980204     C                   MOVEL     'F'           WDAFOR
068100980204     C                   END
068200980204     C                   END
068300990610     C   28              GOTO      FINCT2
068400970414     C*
068500970612     C     FINCT2        ENDSR
068600970612     C*--------------------------------------------------------------*
068700970612     C* CALPGM: Eseguo Call a pgm
068800970612     C*--------------------------------------------------------------*
068900970612     C     CALPGM        BEGSR
069000970612     C*
069100970612     C*  Imposto DS per richiamo altri pgm
069200970612     C                   CLEAR                   FNLSI1
069300980203     C                   MOVEL     V2CFTX        DI1FTX
069400980203     C                   MOVEL     V2CKSC        DI1KSC
069500970612     C                   MOVEL     V2DKSC        DI1DEC
069600970612     C                   MOVEL     V2CLNP        DI1LNP
069700970612     C                   MOVEL     V2CNRS        DI1NRS
069800970612     C                   MOVEL     V2CNSP        DI1NSP
069900970612     C                   MOVEL     V2CAAS        DI1AAS
070000970612     C                   MOVEL     V2CAAS        DI1DSP
070100970612     C                   MOVE      BLPMGS        DI1DSP
070200970616     C                   MOVE      BLPMGS        DI1DSP
070300971007     C                   MOVE      LAB(XY)       DI1LAB
070400980202     C                   MOVE      V2CSTS        DI1STS
070500980202     C                   MOVE      V2CCOD        DI1CEV
070600980202     C                   MOVE      §EMDES        DI1DEV
070700980203     C                   MOVE      §EMDES        DI1DEV
070800980203     C                   MOVE      §EMCOD        DI1C2A
070900980203     C                   MOVE      §EMCCA        DI1CCA
071000980203     C                   MOVE      §EMBLP        DI1BLP
071100980203     C                   MOVE      §EMBLT        DI1BLT
071200980203     C                   MOVE      §EMFOR        DI1FRZ
071300980203     C                   MOVE      §EMTOT        DI1TOT
071400980204     C                   MOVE      WDAFOR        DI1FOR
071500980204     C                   MOVE      BLPNCL        DI1NCL
071600970612     C                   MOVEL     FNLSI1        KPJBU
071700980202     C* Richiamo pgm gestione dettaglio segnacolli
071800980202     C     §EMTOT        IFNE      'S'
071900980202     C                   CALL      'FNLSI1R2'
072000980202     C                   PARM                    KPJBA
072100980202     C                   MOVEL     KPJBU         FNLSI1
072200980202     C                   END
072300980202     C* Se ho digitato F3 esco
072400980202     C     DI1CMD        IFEQ      '03'
072500980202     C                   MOVEL     'S'           WFINE
072600980202     C                   GOTO      FINRIC
072700980202     C                   END
072800980204     C* Se ho digitato F12 azzero campo scelta
072900980204     C     DI1CMD        IFEQ      '12'
073000980204     C                   GOTO      FINRIC
073100980204     C                   END
073200980202     C* Richiamo pgm gestione terza videata: SPEDIZIONE
073300970612     C                   CALL      'FNLSI1R1'
073400970612     C                   PARM                    KPJBA
073500970617     C                   MOVEL     KPJBU         FNLSI1
073600970612     C*
073700970612     C* Se ho digitato F3 esco
073800970612     C                   SELECT
073900970612     C     DI1CMD        WHENEQ    '03'
074000970619     C                   MOVEL     'S'           WFINE
074100970619     C* Controllo se devo richiamare TRTC81
074200970619     C     WRIC81        IFEQ      'S'
074300970619     C                   EXSR      CALL81
074400970619     C                   END
074500970612     C* Se ho digitato F12 azzero campo scelta
074600970612     C     DI1CMD        WHENEQ    '12'
074700970616     C* CONFERMA
074800970616     C     DI1CMD        WHENEQ    '06'
074900970617     C                   MOVEL     'S'           WRIC81
075000970616     C* Aggiorno BLP/BLT
075100980202     C     §EMBLP        IFEQ      'S'
075200970612     C                   CALL      'FNLSI1R3'
075300970617     C                   PARM                    KPJBA
075400970616     C                   END
075500080421      * l'EDIVAC viene sostituito con il TIVGD Download standard
075600080421     C* Scrivo EDVAC --> TIVGD00F
075700970612     C                   CALL      'FNLSI1R4'
075800970612     C                   PARM                    KPJBA
075900970616     C                   ENDSL
076000970612     C*
076100980202     C     FINRIC        ENDSR
076200970617     C*--------------------------------------------------------------*
076300980126     C* CALL81: PER E.D.I. RICHIAMO PGM TRTC81C
076400970617     C*--------------------------------------------------------------*
076500970617     C     CALL81        BEGSR
076600970617     C*
076700980126      * CALL A TRTC81C PER E.D.I.
076800980126     C     §PTFTT        IFNE      'S'
076900061124     C     §PTFTT        ANDNE     'P'
077000980126     C                   MOVEL     'S'           WCAL81            1
077100080421      *
077200080421      *  Scrive IFTSTA x EDI:
077300080421      * ? il TRTC81C  attiva giro di trasm con SNDIFTSTA  ?
077400080421     C                   MOVEL     *all'0'       Elab_Cliente
077500080421     C                   MOVE      CKS(XX)       Elab_Cliente
077600080421     c                   eval      Tipo_Scarico = CSD(XX)
077700080421      *
077800080421     c                   eval      Tipo_File = 'VC'
077900110630     C                   CALL      'TRTCT81C'
078000080421     c                   parm                    Elab_Cliente      8
078100080422     c                   parm                    Tipo_File         2
078200080421     c                   parm                    Tipo_Scarico      2
078300980126      *
078400980126     C                   END
078500980126      *
078600970617     C                   ENDSR
078700970410     C*--------------------------------------------------------------*
078800970410     C* *INZSR: Operazioni iniziali
078900970410     C*--------------------------------------------------------------*
079000970410     C     *INZSR        BEGSR
079100970410     C*
079200970410     C     *ENTRY        PLIST
079300970410     C                   PARM                    KPJBA
079400990223     C* Richiamo xparut
079500970410     C                   Z-ADD     1             CODUT
079600970410     C                   CALL      'X§PARUT'
079700970410     C                   PARM                    UTEDSE
079800970410     C                   MOVEL     REC80         CNCR80
079900970410     C                   MOVEL     RAGUT         RSUT
080000030924      *
080100030924      *  In base all'utente prende il P.O.
080200030924     C                   CLEAR                   Tibs36Ds
080300030924     c                   EVAL      I36ute = Knmus
080400030924     c                   EVAL      I36Tla = 'L'
080500030924     C                   CALL      'TIBS36R'
080600030924     C                   PARM                    Tibs36Ds
080700030924      *
080800970612     C*--- RICERCA CAPOCONTI------------------------------------------*
080900030116     C                   DO        50            X                 5 0
081000970612     C                   MOVE      TCU(X)        TCUDS
081100970612     C     F56           IFEQ      'CG'
081200970612     C     F4            ANDEQ     '1'
081300970612     C     F3            ANDEQ     '0'
081400970612     C                   Z-ADD     KCU(X)        CLIITA            4 0
081500970612     C                   END
081600970612     C                   END
081700970612     C*--- RICERCA CAPOCONTI------------------------------------------*
081800970410      *  Definizione chiavi
081900970410     C     KBLP          KLIST
082000970410     C                   KFLD                    KAAS
082100970410     C                   KFLD                    KLNP
082200970410     C                   KFLD                    KNRS
082300970410     C                   KFLD                    KNSP
082400970410     C     KBLT          KLIST
082500970410     C                   KFLD                    KAAS
082600970410     C                   KFLD                    KLNP
082700970410     C                   KFLD                    KNRS
082800970410     C                   KFLD                    KNSP
082900970410     C     KLBL1         KLIST
083000970410     C                   KFLD                    KAAP
083100970410     C                   KFLD                    KLPP
083200970410     C                   KFLD                    KNRP
083300970410     C                   KFLD                    KNSP
083400970410     C     KLBL2         KLIST
083500970410     C                   KFLD                    KAAN
083600970410     C                   KFLD                    KLPN
083700970410     C                   KFLD                    KNRN
083800970410     C                   KFLD                    KNSN
083900970410     C     KTAB1         KLIST
084000970410     C                   KFLD                    KKUT
084100970410     C                   KFLD                    KCOD
084200970410     C     KTAB2         KLIST
084300970410     C                   KFLD                    KKUT
084400970410     C                   KFLD                    KCOD
084500970410     C                   KFLD                    KKEY
084600970612     C     KACO          KLIST
084700970612     C                   KFLD                    KKUT
084800970612     C                   KFLD                    KKCC
084900970612     C                   KFLD                    KKSC
085000980204     C     KEVB          KLIST
085100980204     C                   KFLD                    KAA2
085200980204     C                   KFLD                    KLNP
085300980204     C                   KFLD                    KNRS
085400980204     C                   KFLD                    KNSP
085500980204     C                   KFLD                    KCEV
085600080918     C     KTBE1         KLIST
085700080918     C                   KFLD                    TBECOD
085800080918     C                   KFLD                    TBEKE1
085900970410      *  Definizione variabili
086000980204     C     *LIKE         DEFINE    EVBAAS        KAA2
086100980204     C     *LIKE         DEFINE    EVBCEV        KCEV
086200970410     C     *LIKE         DEFINE    BLPAAS        KAAS
086300970410     C     *LIKE         DEFINE    BLPLNP        KLNP
086400970410     C     *LIKE         DEFINE    BLPNRS        KNRS
086500970410     C     *LIKE         DEFINE    BLPNSP        KNSP
086600970410     C     *LIKE         DEFINE    LBLAAP        KAAP
086700970410     C     *LIKE         DEFINE    LBLLPP        KLPP
086800970410     C     *LIKE         DEFINE    LBLNRP        KNRP
086900970410     C     *LIKE         DEFINE    LBLAAN        KAAN
087000970410     C     *LIKE         DEFINE    LBLLPN        KLPN
087100970410     C     *LIKE         DEFINE    LBLNRN        KNRN
087200970410     C     *LIKE         DEFINE    LBLNSN        KNSN
087300970410     C     *LIKE         DEFINE    TBLKUT        KKUT
087400970410     C     *LIKE         DEFINE    TBLCOD        KCOD
087500970410     C     *LIKE         DEFINE    TBLKEY        KKEY
087600970611     C     *LIKE         DEFINE    TABCOD        KCODE
087700970612     C     *LIKE         DEFINE    ACOKCC        KKCC
087800970612     C     *LIKE         DEFINE    ACOKSC        KKSC
087900970410      *
088000970410      *  Reperisco la data del giorno
088100970410     C                   TIME                    WHHDAT           14 0
088200970414     C                   MOVE      WHHDAT        G02DAT
088300970414     C                   MOVEL     *BLANK        G02ERR
088400970414     C                   CALL      'XSRDA8'
088500970414     C                   PARM                    WLBDA8
088600970414     C                   Z-ADD     G02INV        OGGI              8 0
088700970611     C*
088800970611     C* Carico dati DS partner in schiera
088900970612     C                   OPEN      EDTAB01L
089000970611     C                   Z-ADD     1             X
089100970611     C                   MOVEL     'PT'          KCODE
089200970616     C     KCODE         CHAIN     EDTAB01L                           30
089300970611     C     *IN30         DOWEQ     '0'
089400970611     C     TABFLG        IFEQ      ' '
089500970611     C                   ADD       1             X
089600970611     C                   MOVEL     TABUNI        DSPT
089700970611     C                   MOVEL     §PTKSC        KSC(X)
089800970616     C                   MOVEL     §PTLNP        LNP(X)
089900970616     C                   MOVEL     §PTNRS        NRS(X)
090000970611     C                   END
090100970616     C     KCODE         READE     EDTAB01L                               30
090200970611     C                   END
090300970616     C*
090400970616     C                   Z-ADD     1             X
090500970616     C                   MOVEL     'PU'          KCODE
090600970616     C     KCODE         CHAIN     EDTAB01L                           30
090700970616     C     *IN30         DOWEQ     '0'
090800970616     C     TABFLG        IFEQ      ' '
090900970616     C                   ADD       1             X
091000970616     C                   MOVEL     TABUNI        DSPU
091100970616     C                   MOVEL     §PULAB        LAB(X)
091200970617     C                   MOVEL     §PUTPC        TPC(X)
091300970616     C                   END
091400970616     C     KCODE         READE     EDTAB01L                               30
091500970616     C                   END
091600980202     C*
091700980202     C* Carico in schiera tabella eventi manuali
091800980202     C                   Z-ADD     0             X
091900030116     C                   Z-ADD     0             X1                5 0
092000980202     C                   MOVEL     'EM'          KCODE
092100980202     C     KCODE         CHAIN     EDTAB01L                           30
092200980202     C     *IN30         DOWEQ     '0'
092300980202     C     TABFLG        IFEQ      ' '
092400980202     C                   MOVEL     TABUNI        DSEM
092500980202     C     §EMLAB        IFEQ      'S'
092600980202     C                   ADD       1             X
092700980204     C                   MOVEL     TABKEY        ELB(X)
092800980202     C                   MOVEL     TABUNI        DLB(X)
092900980202     C                   END
093000980202     C     §EMVGL        IFEQ      'S'
093100980202     C                   ADD       1             X1
093200980204     C                   MOVEL     TABKEY        EVG(X1)
093300980202     C                   MOVEL     TABUNI        DVG(X1)
093400980202     C                   END
093500980202     C     KCODE         READE     EDTAB01L                               30
093600980202     C                   END
093700980202     C                   END
093800970611     C* close EDTAB
093900970611     C                   CLOSE     EDTAB01L
094000970611     C*
094100970410     C* Eseguo caricamento tabella 3K
094200970612     C                   OPEN      TABEL00F
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
095300970617     C                   MOVEL     §3KCKS        CKS(X)
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
097200970701     C*
097300970701     C* Eseguo caricamento da tabella 7J codici pistole fittizie
097400970701     C                   MOVEA     *HIVAL        C7J
097500970701     C                   Z-ADD     0             X
097600970701     C                   Z-ADD     1             KKUT
097700970701     C                   MOVEL     '7J'          KCOD
097800970701     C     KTAB1         CHAIN     TABEL                              30
097900970701     C     *IN30         DOWEQ     '0'
098000970721     C     X             ANDLT     500
098100970701     C     TBLFLG        IFEQ      *BLANKS
098200970701     C                   MOVEL     TBLUNI        DS7J
098300970912     C     §7JRPS        IFEQ      'A'
098400970701     C                   ADD       1             X
098500970701     C                   MOVEL     TBLKEY        C7J(X)
098600970701     C                   END
098700970701     C                   END
098800970701     C     KTAB1         READE     TABEL                                  30
098900970701     C                   END
099000970410     C* Eseguo caricamento £1
099100020506     c                   clear                   trul06ds
099200020506     c                   eval      d06cod = '£1'
099300030924     c                   movel     o36pou        d06key
099400020506     c                   movel     trul06ds      kpjbu
099500020506     c                   call      'TRUL06R'
099600020506     c                   parm                    kpjba
099700020506     c                   movel     kpjbu         trul06ds
099800020506     c                   movea     lin           £1
099900020912      *
100000971008     C* Imposto a 'N' flag richiamo TRTC81C
100100971008     C                   MOVEL     'N'           WCAL81            1
100200970410     C* Eseguo close del file
100300970410     C                   CLOSE     TABEL00F
100400970410     C*
100500970410     C                   ENDSR
100600080930      *------------------------------------------------------------------------*
100700080930      * Routine di controllo x eventuali Errori
100800080930      *------------------------------------------------------------------------*
100900080930     C     *PSSR         BEGSR
101000080930      *
101100080930      * Comunque eseguo x rilasciare il TIVGD.
101200080930      *? Infine elimino il blocco elaborazione TIVGD x tipo file qui.  ?
101300080930     C                   clear                   trul47ds
101400080930     C                   eval      d47opz  = 'F'
101500080930     C                   eval      d47tip  = 'VC'
101600080930     C                   call      'TRUL47R'
101700080930     C                   parm                    trul47ds
101800080930      *
101900080930     C                   ENDSR     '*CANCL'
102000080930     C*------------------------------------------------------------------------*
102100971008**
102200970701Immettere codice cliente                                                      01
102300970701Codice cliente inesistente                                                    02
102400060811Codice cliente non presente in tabella 3K o PT                                03
102500970701Un altro utente sta chiudendo le spedizioni del cliente                       04
102600970701Immettere anno della spedizione                                               05
102700970701Immettere linea di partenza della spedizione                                  06
102800970701Linea di partenza inesistente                                                 07
102900970701Linea di partenza non gestita su questo as                                    08
103000970701Immettere numero della spedizione                                             09
103100970701Spedizione inesistente                                                        10
103200980206Bolla mamma: impossibile gestirla                                             11
103300980206Bolla figlia: impossibile gestirla                                            12
103400970701Spedizione già trasmessa alla filiale di arrivo                               13
103500970701Spedizione già trasmessa alla sede                                            14
103600970701Spedizione già trasmessa al cliente                                           15
103700980202Spedizione senza colli: impossibile eseguire chiusura                         16
103800980202Spedizione non appartenente al cliente selezionato                            17
103900980202Il codice selezionato deve appartenere ad un partner                          18
104000980202Codice evento inesistente                                                     19
104100980202Bolla in assegnato: chiusura definitiva possibile solo in arrivo              20
104200980202Codice evento obbligatorio                                                    21
104300980204Spedizione già consegnata                                                     22
104400980204Evento già inviato impossibile rinviarlo                                      23
104500990610Esistono C.A. aperte per la spedizione, codice evento non valido              24
104600131016 ATTENZIONE: più bolle con stesso riferimento!!                               25
104700131016 VERIFICARE se la Spedizione è quella corretta.                               26
