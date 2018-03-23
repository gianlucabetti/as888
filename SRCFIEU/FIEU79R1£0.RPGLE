000100030620     ************************************************************************
000200030624      *   Genera Record di controllo Fatture Partners Estero                *
000300161298      ***********************************************************************
000400981216     H DECEDIT('0,') DATEDIT(*DMY.)
000500161298      *---------------------------------------------------------------------*
000600030624     FTitas30C  iF   E           k DISK
000700030624     FTita430C  iF   E           k DISK
000800030627     FTita730C  iF   E           k DISK
000900030625     FAzOrg01L  iF   E           k DISK
001000030718     FEcCev30C  iF   E           k DISK
001100030627     FTnTbe01L  iF   E           k DISK
001200030718     FTneVA01L  iF   E           k DISK
001300030718     FTneVO01L  iF   E           k DISK
001400040827     FTNECF02l  uF a E           k DISK
001500030620      *
001600091001     D Importo         S             13  5
001700030630     D GRo             S              3    DIM(50) descend
001800030630     D GRp             S              3    DIM(50)
001900091001     D IMr             S             16  5 DIM(50)
002000091001     D IMc             S             16  5 DIM(50)
002100030718      *
002200030721      * Importo Voci spedizione (0)
002300030721    >D EcceV0        E DS                  extname(eccev00f)
002400030721     D Ptc0            S               *   INZ(%ADDR(CEVC01))
002500030721     D CodV0           S              3  0 DIM(50)
002600030721     D                                     BASED(Ptc0)
002700030721     D Pti0            S               *   INZ(%ADDR(CEVI01))
002800090910     D ImpV0           S             13  5 DIM(50)
002900030721     D                                     BASED(Pti0)
003000030721      * Importo Voci spedizione (P)
003100030721    >D EcceVP        E DS                  extname(eccevP0f) prefix(P_)
003200030721     D Ptcv            S               *   INZ(%ADDR(P_CEVC01))
003300030721     D CodVP           S              3  0 DIM(50)
003400030721     D                                     BASED(Ptcv)
003500030721     D PtiV            S               *   INZ(%ADDR(P_CEVI01))
003600090910     D ImpVP           S             13  5 DIM(50)
003700030721     D                                     BASED(PtiV)
003800030627      *
003900030624     D KPJBA         E DS
004000030625     D OG143         E DS
004100070927     D DSBL4E        E DS
004200030625      *
004300030625     D WLBDAT          DS
004400030625     D  G02DAT                 1      8  0
004500030625     D  G02INV                 9     16  0
004600030625     D  G02ERR                17     17
004700030625     D  G02TGI                18     22  0
004800040818      *
004900040818     D  data_Oggi      s                   like(G02INV) INZ(0)
005000040818      *
005100040818     D param           DS
005200041011     D   kAas                         4  0
005300041011     D   kLnp                         3  0
005400041011     D   kNrs                         2  0
005500041011     D   kNsp                         7  0
005600041011     D   kTpr                         3A
005700041011     D   kNfp                        15A
005800041011     D   kDfp                         8  0
005900041011     D   kGrp                         3A
006000041011     D   kImp                        13  3
006100040827      *
006200030721      *---------------------------------------------------------------------*
006300030721     I* DEFINIZIONE DEI TIPI RECORD DEL FILE ECCEV30C
006400030721     IECCEV000      01
006500030721     IECCEVP00      02
006600030620      *---------------------------------------------------------------------*
006700030624     C     *entry        plist
006800030624     C                   parm                    kpjba
006900030624     c                   movel     kpjbu         param
007000040818      *
007100040818      *  imposta UDATE di elaborazione
007200040818     C                   move      *date         G02DAT
007300040818     C                   MOVE      *ZEROS        G02INV
007400040818     C                   MOVE      *BLANKS       G02ERR
007500040818     C                   CALL      'XSRDA8'
007600040818     C                   PARM                    WLBDAT
007700040818     C                   Z-ADD     G02INV        data_oggi
007800030627      *
007900030627     c     ktbe          klist
008000030627     c                   kfld                    tbeCOD
008100030627     c                   kfld                    tbeKE1
008200030627     c                   kfld                    tbeKE2
008300030718      *
008400030718     c     kEVA          klist
008500030722     c                   kfld                    evaVAR
008600030722     c                   kfld                    evaNTW
008700030722     c                   kfld                    evaFIL
008800030718      *
008900030718     c     kEVO          klist
009000030722     c                   kfld                    evoNTW
009100030722     c                   kfld                    evoFIL
009200030721     c                   kfld                    evoVOC
009300030627      *
009400030627     c     ktas1         klist
009500030627     c                   kfld                    kAas
009600030624     c                   kfld                    kLnp
009700030624     c                   kfld                    kNrs
009800030624     c                   kfld                    kNsp
009900030625      *
010000030627     c     ktas3         klist
010100030625     c                   kfld                    tasAas
010200030625     c                   kfld                    tasLnp
010300030625     c                   kfld                    tasNrs
010400030625     c                   kfld                    tasNsp
010500030625     c                   kfld                    tasTbl
010600030624      *
010700040827     c     kECF          klist
010800040827     c                   kfld                    kAas
010900040827     c                   kfld                    kLnp
011000040827     c                   kfld                    kNrs
011100040827     c                   kfld                    kNsp
011200040827     c                   kfld                    kgrp
011300040827     c                   kfld                    ktpr
011400040827      *
011500040827     c     ktas1         CHAIN     titas30c
011600040827     c                   IF        %FOUND(titas30c)
011700030627     c                   exsr      campi_ECF
011800040827     C                   End
011900030624      *
012000030620     c                   seton                                        LR
012100030627      *----------------------------------------------------------------
012200030627      * imposta i campi per la scrittura
012300030627      *----------------------------------------------------------------
012400030627     c     campi_ECF     Begsr
012500030627      *
012600150318     c     kecf          setll     tnecf02l
012700150318      *
012800041103     c     riprova       tag
012900150318     c     kecf          reade     tnecf02l                             33
013000150318     c**** kecf          chain     tnecf02l                           33
013100041103      *
013200041103      * non deve essere consolidato
013300041103     c  N33              if        ecfdtg > 0
013400041103     c                   goto      riprova
013500041103     c                   end
013600041103      *
013700040827     c   33              clear                   tnecf000
013800040827      *
013900030630     c                   clear                   Gro
014000030630     c                   clear                   Grp
014100030627     c                   clear                   ImR
014200030627     c                   clear                   ImC
014300040116      *
014400040116      * Decodifica il Network
014500040116     c                   exsr      NetW
014600040609      *
014700040609      *  Se si tratta di Partners Euroexpress nel NTW imposto la Linea
014800040609      *   per poter distinguere di quale Partner si tratta
014900040609     c                   if         ecfNTW = 'EEX'
015000040609     c                   move      Linea_Cod     ecfNTW
015100040609     c                   end
015200030627      *
015300030627     c                   movel     tasAAS        ECFAAS
015400030627     c                   movel     tasLNP        ECFLNP
015500030627     c                   movel     tasNRS        ECFNRS
015600030627     c                   movel     tasNSP        ECFNSP
015700030627     c                   movel     tasLNA        ECFLNA
015800030627     c                   movel     tasMGS        ECFMGS
015900030627     c                   movel     tasTBL        ECFTBL
016000030627     c                   movel     tasFBR        ECFFBR
016100030627     c                   movel     tasKSC        ECFKSC
016200030627     c                   movel     tasDTI        ECFDTI
016300030627     c                   movel     tasDCM        ECFDCM
016400030627     c                   movel     tasCCA        ECFCCA
016500030627     c                   movel     tasNCL        ECFNCL
016600030627     c                   movel     tasPKF        ECFPKF
016700030627     c                   movel     tasVLF        ECFVLF
016800030627     c                   movel     tasRMN        ECFRMN
016900030703      *
017000030703      * dta e nr.fattura del Partner
017100030707     c                   movel     tasNFT        ECFNFT
017200030707     c                   movel     tasDFT        ECFDFT
017300030627     c                   exsr      Rif_Partner
017400030630      *
017500030630     c                   movel     *zeros        ECFIMP
017600041015      *
017700041104     C                   Z-ADD     data_Oggi     ECFdta
017800041104      *
017900041104      * x non trovato non deve incasellare
018000041104     c                   if        kTpr = 'NTR'
018100041104     C                   Z-ADD     0             ECFdta
018200041104     c                   end
018300030627      * Imposta i Ricavi
018400030627     c                   exsr      Ricavi
018500030627      * Imposta le Competenze
018600030627     c                   exsr      Competenze
018700030627      *
018800040819     c                   z-add     kImp          ecfIMP
018900040819     c                   z-add     kDfp          ecfDFP
019000040819     c                   movel     kNfp          ecfNFP
019100040819     ******>>>>>>
019200040827     c                   movel     Kgrp          ECFGRP
019300030630      *
019400030630     c                   z-add     1             Ir
019500040827     c     kgrp          lookup    Grp(Ir)                                21
019600030630      * Ricavi/Competenze
019700091001     c   21              z-add(h)  ImR(Ir)       ECFIMR
019800091001     c   21              z-add(h)  ImC(Ir)       ECFIMC
019900041015      *
020000041015      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
020100040827      * Al momento
020200041015      *  Non devono essere scritti i ricavi
020300041015     c                   clear                   ECFIMR
020400041015      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
020500040827      *
020600040818      *  scrive impostando la tipologia passata dai prametri di ingresso
020700040818     c                   movel     kTpr          ecfTPR
020800040818      *
020900040827     c   33              write     tnecf000
021000040827     c  N33              update    tnecf000
021100030630      *
021200030627     c                   Endsr
021300030627      *----------------------------------------------------------------
021400030627      *  Riferimento Partner Estero solo se una Partenza
021500030627      *----------------------------------------------------------------
021600030627     c     Rif_Partner   Begsr
021700030829      *
021800030829      *  Se è un Import DPD si deve impostare il Rif.Numerico
021900030829     c                   iF        §ogNTW = 'DPD' and ImpExp = 'I'
022000030829      *
022100030829     c                   if        ecfRMN > 100000000000 and
022200030829     c                             ecfRMN < 999999999999
022300030829     c                   move      ecfRMN        fld12            12
022400030829     c                   movel     fld12         fld11            11
022500030829     c                   movel     fld11         ecfRIF
022600030829     c                   end
022700030829      *
022800030829     c                   eLSe
022900030627      *
023000030627      *  RIFERIMENTO Mittente  da tita4
023100030627     c     ktas1         setll     tita430c
023200030627     c     ktas1         reade     tita430c
023300030627      *
023400030627     c                   dow       not %Eof(tita430c)
023500030627      *
023600030829      * Se è un DPD x LNP/Network
023700030828     c                   if        §ogNTW = 'DPD'
023800030828      *
023900030829      *  Se è un IMPORT DPD non deve prendere da TITA4 ma dal RMN gli 11
024000030829      *   caratteri.
024100060626     c******             if        ta4TRC = 'F' and ImpExp = 'E'
024200060626     c                   if        ta4TRC = 'I' and ImpExp = 'E'
024300030829     c*******                      or ta4TRC = 'A' and ImpExp = 'I'
024400060626     c                   eval      ECFRIF =  %subst(ta4not:1:14)
024500030627     c                   end
024600030828      *
024700030828     c                   else
024800030828      *
024900030828      * Se è un Fedex x LNP/Network
025000070927     c******             if        ta4TRC = 'A'
025100070927     c                   if        ta4TRC = 'E'
025200070927     c*****              eval      ECFRIF = %subst(ta4not:16:20)
025300070927     c                   movel     ta4not        dsbl4E
025400070927     c                   movel     §b4ERP        ecfRIF
025500030828      *  se non c'è il riferimento allora imposta il rif.numerico
025600030828     c                   if        ecfRIF = *blank
025700030828     c                   movel     ecfRMN        ecfRIF
025800030828     c                   end
025900030828     c                   end
026000030828      *
026100030828     c                   end
026200030627      *
026300030627     c     ktas1         reade     tita430c
026400030627     c                   enddo
026500030828      *
026600030828      *  se Non trova un riferimento prende il rif_numerico
026700030828     c                   if        ecfRIF = *blank
026800030828     c                   if        §ogNTW = 'DPD'
026900030828     c*
027000030828     c                   if        ecfRMN > 100000000000 and
027100030828     c                             ecfRMN < 999999999999
027200030828     c                   move      ecfRMN        fld12            12
027300030828     c                   movel     fld12         fld11            11
027400030828     c                   movel     fld11         ecfRIF
027500030828     c                   else
027600030828     c*******            movel     ecfRMN        ecfRIF
027700030828     c                   end
027800030828     c*
027900030828     c                   else
028000030828     c                   movel     ecfRMN        ecfRIF
028100030828     c                   end
028200030828     c                   end
028300030828      *
028400030829     c                   eNd
028500030829      *
028600030627     c                   Endsr
028700030627      *----------------------------------------------------------------
028800030627      *  imposta i Ricavi raggruppati su schiere
028900030627      *----------------------------------------------------------------
029000030627     c     Ricavi        Begsr
029100030627      *
029200030627      * IMPORTO Ricavo
029300030627      *  Porto
029400030627     c                   if        tasPOR > 0
029500030722     c                   clear                   evaVAR
029600030722     c                   movel     '1'           evaVAR
029700030627     c                   z-add     tasPOR        importo
029800030718     c                   exsr      Tbe_EVA
029900030627     c                   end
030000030627      *  Varie 1
030100030627     c                   if        tasVA1 > 0
030200030722     c                   clear                   evaVAR
030300030722     c                   movel     tasSV1        evaVAR
030400030627     c                   z-add     tasVA1        importo
030500030718     c                   exsr      Tbe_EVA
030600030627     c                   end
030700030627      *  Varie 2
030800030627     c                   if        tasVA2 > 0
030900030722     c                   clear                   evaVAR
031000030722     c                   movel     tasSV2        evaVAR
031100030627     c                   z-add     tasVA2        importo
031200030718     c                   exsr      Tbe_EVA
031300030627     c                   end
031400030627      *  Varie 3
031500030627     c                   if        tasVA3 > 0
031600030722     c                   clear                   evaVAR
031700030722     c                   movel     tasSV3        evaVAR
031800030627     c                   z-add     tasVA3        importo
031900030718     c                   exsr      Tbe_EVA
032000030627     c                   end
032100030627      *  Altre Varie
032200030627      *
032300030627      * Controlla quindi sul file VARIE se ci sono altre varie da aggiungere
032400030627      *  e raggruppare.
032500030627     c     ktas3         setll     tita730c
032600030627     c     ktas3         reade     tita730c
032700030627     c                   dow       not %Eof(tita730c)
032800030722     c                   clear                   evaVAR
032900030722     c                   movel     ta7SVn        evaVAR
033000030627     c                   z-add     ta7VAn        importo
033100030718     c                   exsr      Tbe_EVA
033200030627     c     ktas3         reade     tita730c
033300030627     c                   enddo
033400030627      *
033500030627     c                   Endsr
033600030627      *----------------------------------------------------------------
033700030627      *  Decodifica il raggruppamento per le Varie
033800030627      *----------------------------------------------------------------
033900030718     c     TBE_EVA       Begsr
034000030627      *
034100030722     c                   clear                   evaNTW
034200030722     c                   clear                   evaFIL
034300030718     c                   move      '999'         Gruppo            3
034400030721      *
034500030721      * prova se ci sono delle particolarità sul Partner + LNP/A
034600030722      *  chiave completa
034700030722     c                   movel     §ogNtw        evaNTW
034800030722     c                   move      Linea_Cod     evaFIL
034900030721     c     kEVA          setll     tnEVA01l
035000030721     C                   IF        not %EQUAL
035100030721      * se ci sono dei codici particolari da prendere in considerazione
035200030627      *  al posto di quelli standard.
035300030722      *  chiave parziale
035400030722     c                   movel     §ogNtw        evaNTW
035500030722     c                   clear                   evaFIL
035600030718     c     kEVA          setll     tnEVA01l
035700030718     C                   IF        not %EQUAL
035800030722     c                   clear                   evaNTW
035900030722     c                   clear                   evaFIL
036000030722     c     kEVA          setll     tnEVA01l
036100030718     c                   end
036200030721     c                   endIF
036300030627      *
036400030718     C                   IF        %EQUAL
036500030718      *
036600030718     C                   DOU       %EOF
036700030718     C     kEVA          Reade     tnEVA01l
036800030718     C                   IF        NOT %EOF and
036900041011     C                             evaDDA <= data_oggi and
037000041011     C                             evaADA >= data_oggi
037100030718     c                   move      evaGRP        Gruppo
037200030718     c                   leave
037300030718     C                   ENDIF
037400030718     C                   ENDDO
037500030718      *
037600030718     c                   end
037700030718      *
037800030627      * Incasella l'importo del ricavo nel gruppo di appartenenza
037900030718      *  legato alla tabella EVA.
038000030627     c                   z-add     1             Ir                3 0
038100030718     c     Gruppo        lookup    Grp(Ir)                                21
038200030627     c  n21              z-add     1             Ir
038300030630     c  n21*blank        lookup    Grp(Ir)                                21
038400030718     c                   movel     Gruppo        Grp(Ir)
038500030627     c                   add       Importo       ImR(Ir)
038600030627      *
038700030627     c                   Endsr
038800030627      *----------------------------------------------------------------
038900030627      *  imposta le Competenze raggruppate su schiere
039000030627      *----------------------------------------------------------------
039100030627     c     Competenze    Begsr
039200030627      *
039300030721     c                   clear                   ImpV0
039400030721     c                   clear                   ImpVP
039500030721     c                   clear                   CodV0
039600030721     c                   clear                   CodVP
039700030721      *
039800030627      * IMPORTO Competenze
039900030718     c     ktas3         chain     Eccev30C
040000030718     c                   if        %Found(Eccev30C)
040100030627      *
040200030721     C                   Do        50            CV                3 0
040300030721      *
040400030721     C                   If        *in01 = *on
040500030721      * esce quando non ha più Voci in skiera
040600030721     C                   If        CodV0(CV) = 0
040700030721     c                   leave
040800030721     c                   end
040900030721     c                   clear                   evoVOC
041000030721     c                   movel     CodV0(CV)     evoVOC
041100030721     c                   z-add     ImpV0(CV)     importo
041200030721     c                   exsr      Tbe_EVO
041300030721     c                   end
041400030718      *
041500030721     C                   If        *in02 = *on
041600030721      * esce quando non ha più Voci in skiera
041700030721     C                   If        CodVP(CV) = 0
041800030721     c                   leave
041900030721     c                   end
042000030721     c                   clear                   evoVOC
042100030721     c                   movel     CodVP(CV)     evoVOC
042200030721     c                   z-add     ImpVP(CV)     importo
042300030721     c                   exsr      Tbe_EVO
042400030721     c                   end
042500030721      *
042600030721     c                   endDo
042700030627      *
042800030721     C                   endIF
042900030721      *
043000030627     c                   Endsr
043100030627      *----------------------------------------------------------------
043200030627      *  Decodifica il raggruppamento per le Voci
043300030627      *----------------------------------------------------------------
043400030718     c     TBE_EVO       Begsr
043500030722      *
043600030722     c                   clear                   evoNTW
043700030722     c                   clear                   evoFIL
043800030722     c                   move      '999'         Gruppo            3
043900030722      *
044000030722      * prova se ci sono delle particolarità sul Partner + LNP/A
044100030722      *  chiave completa
044200030722     c                   movel     §ogNtw        evoNTW
044300030722     c                   move      Linea_Cod     evoFIL
044400030718     c     kEVO          setll     tnEVO01l
044500030718     C                   IF        not %EQUAL
044600030722      * se ci sono dei codici particolari da prendere in considerazione
044700030722      *  al posto di quelli standard.
044800030722      *  chiave parziale
044900030723     c                   movel     §ogNtw        evoNTW
045000030723     c                   clear                   evoFIL
045100030718     c     kEVO          setll     tnEVO01l
045200030718     C                   IF        not %EQUAL
045300030723     c                   clear                   evoNTW
045400030723     c                   clear                   evoFIL
045500030722     c     kEVO          setll     tnEVO01l
045600030718     c                   end
045700030718     c                   endIF
045800030718      *
045900030718     C                   IF        %EQUAL
046000030718      *
046100030718     C                   DOU       %EOF
046200030718     C     kEVO          Reade     tnEVO01l
046300030718     C                   IF        NOT %EOF        and
046400041011     C                             evoDDA <= data_oggi and
046500041011     C                             evoADA >= data_oggi
046600030718     c                   move      evoGRP        Gruppo
046700030718     c                   leave
046800030718     C                   ENDIF
046900030718     C                   ENDDO
047000030627      *
047100030627      * Incasella l'importo del ricavo nel gruppo di appartenenza
047200030718      *  legato alla tabella EVO.
047300030627     c                   z-add     1             Ic                3 0
047400030718     c     Gruppo        lookup    Grp(Ic)                                21
047500030627     c  n21              z-add     1             Ic
047600030630     c  n21*blank        lookup    Grp(Ic)                                21
047700030718     c                   movel     Gruppo        Grp(Ic)
047800030627     c                   add       Importo       ImC(Ic)
047900030721      *
048000030721     c                   end
048100030703      *
048200030627     c                   Endsr
048300030627      *----------------------------------------------------------------
048400030627      *  Decodifica il Network
048500030627      *----------------------------------------------------------------
048600030627     c     Netw          Begsr
048700030627      *
048800030828     c                   clear                   ImpExp            1
048900030627      *
049000030627      *  x LNP controlla Estero
049100030627     C                   clear                   OG143
049200030627     c     tasLNP        chain     AzOrg01L
049300030627     c                   if        %Found(AzOrg01L)
049400030627     C                   movel     ORGDE3        OG143
049500030627     c                   end
049600030627      *
049700030627      * Attenzione: il Network estero può essere x LNP o x LNA a seconda
049800030627      *  si tratti di IMPORT o EXPORT quindi avendo prima controllato per LNP,
049900030627      *  se non fosse risultato tale allora deve controllare x LNA.
050000030627     c                   if         §ogNTW = 'FED' or
050100030627     c                              §ogNTW = 'EEX' or
050200030627     c                              §ogNTW = 'DPD'
050300030627     c                   movel     §ogNTW        ECFNTW
050400030703      *  quindi le competenze da tenere in considerazione per il confronto sono
050500030703      *  di arrivo.
050600030828     c                   movel     'I'           ImpExp
050700030721     c                   movel     'LNP'         Estero_LN         3
050800030721     c                   z-add     tasLNP        Linea_Cod         3 0
050900030627      *
051000030627     c                   else
051100030627     C                   clear                   OG143
051200030703      * prova per LNA -> se prima, x LNP il network non è un estero
051300030627     c     tasLNA        chain     AzOrg01L
051400030627     c                   if        %Found(AzOrg01L)
051500030627     C                   movel     ORGDE3        OG143
051600030627     c                   movel     §ogNTW        ECFNTW
051700030703      *  quindi le competenze da tenere in considerazione per il confronto sono
051800030703      *  di partenza.
051900030828     c                   movel     'E'           ImpExp
052000030721     c                   movel     'LNA'         Estero_LN
052100030721     c                   z-add     tasLNA        Linea_Cod
052200030627     c                   end
052300030627     c                   end
052400030627      *
052500030627     c                   Endsr
052600030627      *----------------------------------------------------------------
