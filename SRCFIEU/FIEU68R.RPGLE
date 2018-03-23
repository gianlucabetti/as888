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
001400030627     FTNECF00F  O  a E             DISK
001500030620      *
001600030627     D Importo         S             13  3
001700030630     D GRo             S              3    DIM(50) descend
001800030630     D GRp             S              3    DIM(50)
001900030627     D IMr             S             16  3 DIM(50)
002000030627     D IMc             S             16  3 DIM(50)
002100030718      *
002200030721      * Importo Voci spedizione (0)
002300030721    >D EcceV0        E DS                  extname(eccev00f)
002400030721     D Ptc0            S               *   INZ(%ADDR(CEVC01))
002500030721     D CodV0           S              3  0 DIM(50)
002600030721     D                                     BASED(Ptc0)
002700030721     D Pti0            S               *   INZ(%ADDR(CEVI01))
002800030721     D ImpV0           S              7  3 DIM(50)
002900030721     D                                     BASED(Pti0)
003000030721      * Importo Voci spedizione (P)
003100030721    >D EcceVP        E DS                  extname(eccevP0f) prefix(P_)
003200030721     D Ptcv            S               *   INZ(%ADDR(P_CEVC01))
003300030721     D CodVP           S              3  0 DIM(50)
003400030721     D                                     BASED(Ptcv)
003500030721     D PtiV            S               *   INZ(%ADDR(P_CEVI01))
003600030721     D ImpVP           S              7  3 DIM(50)
003700030721     D                                     BASED(PtiV)
003800030627      *
003900030625     D OG143         E DS
004000030625      *
004100030625     D WLBDAT          DS
004200030625     D  G02DAT                 1      8  0
004300030625     D  G02INV                 9     16  0
004400030625     D  G02ERR                17     17
004500030625     D  G02TGI                18     22  0
004600030624      *
004700030624     D param           DS
004800030624     D   kAas                  1      4  0
004900030624     D   kLnp                  5      7  0
005000030624     D   kNrs                  8      9  0
005100030624     D   kNsp                 10     16  0
005200030624     D   kTbl                 17     18
005300030703     D   kNtw                 19     21
005400030721      *
005500030721      *---------------------------------------------------------------------*
005600030721     I* DEFINIZIONE DEI TIPI RECORD DEL FILE ECCEV30C
005700030721     IECCEV000      01
005800030721     IECCEVP00      02
005900030620      *---------------------------------------------------------------------*
006000030624     C     *entry        plist
006100040413     C                   parm                    param
006200030627      *
006300030627     c     ktbe          klist
006400030627     c                   kfld                    tbeCOD
006500030627     c                   kfld                    tbeKE1
006600030627     c                   kfld                    tbeKE2
006700030718      *
006800030718     c     kEVA          klist
006900030722     c                   kfld                    evaVAR
007000030722     c                   kfld                    evaNTW
007100030722     c                   kfld                    evaFIL
007200030718      *
007300030718     c     kEVO          klist
007400030722     c                   kfld                    evoNTW
007500030722     c                   kfld                    evoFIL
007600030721     c                   kfld                    evoVOC
007700030627      *
007800030627     c     ktas1         klist
007900030627     c                   kfld                    kAas
008000030624     c                   kfld                    kLnp
008100030624     c                   kfld                    kNrs
008200030624     c                   kfld                    kNsp
008300030624      *
008400030627     c     ktas2         klist
008500030624     c                   kfld                    kAas
008600030624     c                   kfld                    kLnp
008700030624     c                   kfld                    kNrs
008800030624     c                   kfld                    kNsp
008900030624     c                   kfld                    kTbl
009000030625      *
009100030627     c     ktas3         klist
009200030625     c                   kfld                    tasAas
009300030625     c                   kfld                    tasLnp
009400030625     c                   kfld                    tasNrs
009500030625     c                   kfld                    tasNsp
009600030625     c                   kfld                    tasTbl
009700030624      *
009800030624     c                   if        kTbl = *blank
009900030627     c     ktas1         setll     titas30c
010000030627     c     ktas1         reade     titas30c
010100030624     c                   else
010200030627     c     ktas2         setll     titas30c
010300030627     c     ktas2         reade     titas30c
010400030624     c                   end
010500030624      *
010600030624     c                   dow       not %Eof(titas30c)
010700030625      *
010800030627     c                   exsr      campi_ECF
010900030625      *
011000030624     c                   if        kTbl = *blank
011100030627     c     ktas1         reade     titas30c
011200030624     c                   else
011300030627     c     ktas2         reade     titas30c
011400030624     c                   end
011500030627      *
011600030624     C                   Enddo
011700030624      *
011800030620     c                   seton                                        LR
011900030627      *----------------------------------------------------------------
012000030627      * imposta i campi per la scrittura
012100030627      *----------------------------------------------------------------
012200030627     c     campi_ECF     Begsr
012300030627      *
012400030627     c                   clear                   tnecf000
012500030630     c                   clear                   Gro
012600030630     c                   clear                   Grp
012700030627     c                   clear                   ImR
012800030627     c                   clear                   ImC
012900040116      *
013000040116      * Decodifica il Network
013100040116     c                   exsr      NetW
013200040116     c                   if        KNtw <> *blank and
013300040116     c                             Kntw <> ecfNTW
013400040116     c                   goto      No_write
013500040116     c                   End
013600030627      *
013700030627      *  imposta UDATE
013800030627     C                   move      *date         G02DAT
013900030627     C                   MOVE      *ZEROS        G02INV
014000030627     C                   MOVE      *BLANKS       G02ERR
014100030627     C                   CALL      'XSRDA8'
014200030627     C                   PARM                    WLBDAT
014300030627     C                   Z-ADD     G02INV        ECFDTG
014400030627      *
014500030627     c                   movel     tasAAS        ECFAAS
014600030627     c                   movel     tasLNP        ECFLNP
014700030627     c                   movel     tasNRS        ECFNRS
014800030627     c                   movel     tasNSP        ECFNSP
014900030627     c                   movel     tasLNA        ECFLNA
015000030627     c                   movel     tasMGS        ECFMGS
015100030627     c                   movel     tasTBL        ECFTBL
015200030627     c                   movel     tasFBR        ECFFBR
015300030627     c                   movel     tasKSC        ECFKSC
015400030627     c                   movel     tasDTI        ECFDTI
015500030627     c                   movel     tasDCM        ECFDCM
015600030627     c                   movel     tasCCA        ECFCCA
015700030627     c                   movel     tasNCL        ECFNCL
015800030627     c                   movel     tasPKF        ECFPKF
015900030627     c                   movel     tasVLF        ECFVLF
016000030627     c                   movel     tasRMN        ECFRMN
016100030703      *
016200030703      * dta e nr.fattura del Partner
016300030707     c                   movel     tasNFT        ECFNFT
016400030707     c                   movel     tasDFT        ECFDFT
016500030627      *
016600030627     c                   exsr      Rif_Partner
016700030630      *
016800030630     c                   movel     *zeros        ECFIMP
016900030630     c                   movel     *zeros        ECFDTA
017000030627      *
017100030627      * Imposta i Ricavi
017200030627     c                   exsr      Ricavi
017300030627      *
017400030627      * Imposta le Competenze
017500030627     c                   exsr      Competenze
017600030627      *
017700030630      * riordina i dati immagazzinati per scrivere i records
017800030630     c                   movea     grp           gro
017900030630     c                   sorta     gro
018000030630      *   deve scrivere tanti records quanti sono i raggruppamenti messi
018100030630      *    nelle schiere
018200030630     c                   do        50            cnt               3 0
018300030630     c                   if        gro(cnt) = *blank
018400030630     c                             and cnt > 1
018500030630     c                   leave
018600030630     c                   end
018700030627      *
018800030723     c                   movel     gro(cnt)      ECFGRP
018900030630      *
019000030630     c                   z-add     1             Ir
019100030630     c     gro(cnt)      lookup    Grp(Ir)                                21
019200030630      * Ricavi/Competenze
019300030630     c                   z-add     ImR(Ir)       ECFIMR
019400030630     c                   z-add     ImC(Ir)       ECFIMC
019500030627      *
019600030627     c                   write     tnecf000
019700030630      *
019800030630     c                   EndDO
019900030703     c     No_Write      tag
020000030627      *
020100030627     c                   Endsr
020200030627      *----------------------------------------------------------------
020300030627      *  Riferimento Partner Estero solo se una Partenza
020400030627      *----------------------------------------------------------------
020500030627     c     Rif_Partner   Begsr
020600030829      *
020700030829      *  Se è un Import DPD si deve impostare il Rif.Numerico
020800030829     c                   iF        §ogNTW = 'DPD' and ImpExp = 'I'
020900030829      *
021000030829     c                   if        ecfRMN > 100000000000 and
021100030829     c                             ecfRMN < 999999999999
021200030829     c                   move      ecfRMN        fld12            12
021300030829     c                   movel     fld12         fld11            11
021400030829     c                   movel     fld11         ecfRIF
021500030829     c                   end
021600030829      *
021700030829     c                   eLSe
021800030627      *
021900030627      *  RIFERIMENTO Mittente  da tita4
022000030627     c     ktas1         setll     tita430c
022100030627     c     ktas1         reade     tita430c
022200030627      *
022300030627     c                   dow       not %Eof(tita430c)
022400030627      *
022500030829      * Se è un DPD x LNP/Network
022600030828     c                   if        §ogNTW = 'DPD'
022700030828      *
022800030829      *  Se è un IMPORT DPD non deve prendere da TITA4 ma dal RMN gli 11
022900030829      *   caratteri.
023000030829     c                   if        ta4TRC = 'F' and ImpExp = 'E'
023100030829     c*******                      or ta4TRC = 'A' and ImpExp = 'I'
023200030627     c                   eval      ECFRIF =  %subst(ta4not:1:11)
023300030627     c                   end
023400030828      *
023500030828     c                   else
023600030828      *
023700030828      * Se è un Fedex x LNP/Network
023800030828     c                   if        ta4TRC = 'A'
023900030828     c                   eval      ECFRIF = %subst(ta4not:16:20)
024000030828      *  se non c'è il riferimento allora imposta il rif.numerico
024100030828     c                   if        ecfRIF = *blank
024200030828     c                   movel     ecfRMN        ecfRIF
024300030828     c                   end
024400030828     c                   end
024500030828      *
024600030828     c                   end
024700030627      *
024800030627     c     ktas1         reade     tita430c
024900030627     c                   enddo
025000030828      *
025100030828      *  se Non trova un riferimento prende il rif_numerico
025200030828     c                   if        ecfRIF = *blank
025300030828     c                   if        §ogNTW = 'DPD'
025400030828     c*
025500030828     c                   if        ecfRMN > 100000000000 and
025600030828     c                             ecfRMN < 999999999999
025700030828     c                   move      ecfRMN        fld12            12
025800030828     c                   movel     fld12         fld11            11
025900030828     c                   movel     fld11         ecfRIF
026000030828     c                   else
026100030828     c*******            movel     ecfRMN        ecfRIF
026200030828     c                   end
026300030828     c*
026400030828     c                   else
026500030828     c                   movel     ecfRMN        ecfRIF
026600030828     c                   end
026700030828     c                   end
026800030828      *
026900030829     c                   eNd
027000030829      *
027100030627     c                   Endsr
027200030627      *----------------------------------------------------------------
027300030627      *  imposta i Ricavi raggruppati su schiere
027400030627      *----------------------------------------------------------------
027500030627     c     Ricavi        Begsr
027600030627      *
027700030627      * IMPORTO Ricavo
027800030627      *  Porto
027900030627     c                   if        tasPOR > 0
028000030722     c                   clear                   evaVAR
028100030722     c                   movel     '1'           evaVAR
028200030627     c                   z-add     tasPOR        importo
028300030718     c                   exsr      Tbe_EVA
028400030627     c                   end
028500030627      *  Varie 1
028600030627     c                   if        tasVA1 > 0
028700030722     c                   clear                   evaVAR
028800030722     c                   movel     tasSV1        evaVAR
028900030627     c                   z-add     tasVA1        importo
029000030718     c                   exsr      Tbe_EVA
029100030627     c                   end
029200030627      *  Varie 2
029300030627     c                   if        tasVA2 > 0
029400030722     c                   clear                   evaVAR
029500030722     c                   movel     tasSV2        evaVAR
029600030627     c                   z-add     tasVA2        importo
029700030718     c                   exsr      Tbe_EVA
029800030627     c                   end
029900030627      *  Varie 3
030000030627     c                   if        tasVA3 > 0
030100030722     c                   clear                   evaVAR
030200030722     c                   movel     tasSV3        evaVAR
030300030627     c                   z-add     tasVA3        importo
030400030718     c                   exsr      Tbe_EVA
030500030627     c                   end
030600030627      *  Altre Varie
030700030627      *
030800030627      * Controlla quindi sul file VARIE se ci sono altre varie da aggiungere
030900030627      *  e raggruppare.
031000030627     c     ktas3         setll     tita730c
031100030627     c     ktas3         reade     tita730c
031200030627     c                   dow       not %Eof(tita730c)
031300030722     c                   clear                   evaVAR
031400030722     c                   movel     ta7SVn        evaVAR
031500030627     c                   z-add     ta7VAn        importo
031600030718     c                   exsr      Tbe_EVA
031700030627     c     ktas3         reade     tita730c
031800030627     c                   enddo
031900030627      *
032000030627     c                   Endsr
032100030627      *----------------------------------------------------------------
032200030627      *  Decodifica il raggruppamento per le Varie
032300030627      *----------------------------------------------------------------
032400030718     c     TBE_EVA       Begsr
032500030627      *
032600030722     c                   clear                   evaNTW
032700030722     c                   clear                   evaFIL
032800030718     c                   move      '999'         Gruppo            3
032900030721      *
033000030721      * prova se ci sono delle particolarità sul Partner + LNP/A
033100030722      *  chiave completa
033200030722     c                   movel     §ogNtw        evaNTW
033300030722     c                   move      Linea_Cod     evaFIL
033400030721     c     kEVA          setll     tnEVA01l
033500030721     C                   IF        not %EQUAL
033600030721      * se ci sono dei codici particolari da prendere in considerazione
033700030627      *  al posto di quelli standard.
033800030722      *  chiave parziale
033900030722     c                   movel     §ogNtw        evaNTW
034000030722     c                   clear                   evaFIL
034100030718     c     kEVA          setll     tnEVA01l
034200030718     C                   IF        not %EQUAL
034300030722     c                   clear                   evaNTW
034400030722     c                   clear                   evaFIL
034500030722     c     kEVA          setll     tnEVA01l
034600030718     c                   end
034700030721     c                   endIF
034800030627      *
034900030718     C                   IF        %EQUAL
035000030718      *
035100030718     C                   DOU       %EOF
035200030718     C     kEVA          Reade     tnEVA01l
035300030718     C                   IF        NOT %EOF and
035400030718     C                             evaDDA <= EcfDtg and
035500030718     C                             evaADA >= EcfDtg
035600030718     c                   move      evaGRP        Gruppo
035700030718     c                   leave
035800030718     C                   ENDIF
035900030718     C                   ENDDO
036000030718      *
036100030718     c                   end
036200030718      *
036300030627      * Incasella l'importo del ricavo nel gruppo di appartenenza
036400030718      *  legato alla tabella EVA.
036500030627     c                   z-add     1             Ir                3 0
036600030718     c     Gruppo        lookup    Grp(Ir)                                21
036700030627     c  n21              z-add     1             Ir
036800030630     c  n21*blank        lookup    Grp(Ir)                                21
036900030718     c                   movel     Gruppo        Grp(Ir)
037000030627     c                   add       Importo       ImR(Ir)
037100030627      *
037200030627     c                   Endsr
037300030627      *----------------------------------------------------------------
037400030627      *  imposta le Competenze raggruppate su schiere
037500030627      *----------------------------------------------------------------
037600030627     c     Competenze    Begsr
037700030627      *
037800030721     c                   clear                   ImpV0
037900030721     c                   clear                   ImpVP
038000030721     c                   clear                   CodV0
038100030721     c                   clear                   CodVP
038200030721      *
038300030627      * IMPORTO Competenze
038400030718     c     ktas3         chain     Eccev30C
038500030718     c                   if        %Found(Eccev30C)
038600030627      *
038700030721     C                   Do        50            CV                3 0
038800030721      *
038900030721     C                   If        *in01 = *on
039000030721      * esce quando non ha più Voci in skiera
039100030721     C                   If        CodV0(CV) = 0
039200030721     c                   leave
039300030721     c                   end
039400030721     c                   clear                   evoVOC
039500030721     c                   movel     CodV0(CV)     evoVOC
039600030721     c                   z-add     ImpV0(CV)     importo
039700030721     c                   exsr      Tbe_EVO
039800030721     c                   end
039900030718      *
040000030721     C                   If        *in02 = *on
040100030721      * esce quando non ha più Voci in skiera
040200030721     C                   If        CodVP(CV) = 0
040300030721     c                   leave
040400030721     c                   end
040500030721     c                   clear                   evoVOC
040600030721     c                   movel     CodVP(CV)     evoVOC
040700030721     c                   z-add     ImpVP(CV)     importo
040800030721     c                   exsr      Tbe_EVO
040900030721     c                   end
041000030721      *
041100030721     c                   endDo
041200030627      *
041300030721     C                   endIF
041400030721      *
041500030627     c                   Endsr
041600030627      *----------------------------------------------------------------
041700030627      *  Decodifica il raggruppamento per le Voci
041800030627      *----------------------------------------------------------------
041900030718     c     TBE_EVO       Begsr
042000030722      *
042100030722     c                   clear                   evoNTW
042200030722     c                   clear                   evoFIL
042300030722     c                   move      '999'         Gruppo            3
042400030722      *
042500030722      * prova se ci sono delle particolarità sul Partner + LNP/A
042600030722      *  chiave completa
042700030722     c                   movel     §ogNtw        evoNTW
042800030722     c                   move      Linea_Cod     evoFIL
042900030718     c     kEVO          setll     tnEVO01l
043000030718     C                   IF        not %EQUAL
043100030722      * se ci sono dei codici particolari da prendere in considerazione
043200030722      *  al posto di quelli standard.
043300030722      *  chiave parziale
043400030723     c                   movel     §ogNtw        evoNTW
043500030723     c                   clear                   evoFIL
043600030718     c     kEVO          setll     tnEVO01l
043700030718     C                   IF        not %EQUAL
043800030723     c                   clear                   evoNTW
043900030723     c                   clear                   evoFIL
044000030722     c     kEVO          setll     tnEVO01l
044100030718     c                   end
044200030718     c                   endIF
044300030718      *
044400030718     C                   IF        %EQUAL
044500030718      *
044600030718     C                   DOU       %EOF
044700030718     C     kEVO          Reade     tnEVO01l
044800030718     C                   IF        NOT %EOF        and
044900030718     C                             evoDDA <= EcfDtg and
045000030718     C                             evoADA >= EcfDtg
045100030718     c                   move      evoGRP        Gruppo
045200030718     c                   leave
045300030718     C                   ENDIF
045400030718     C                   ENDDO
045500030627      *
045600030627      * Incasella l'importo del ricavo nel gruppo di appartenenza
045700030718      *  legato alla tabella EVO.
045800030627     c                   z-add     1             Ic                3 0
045900030718     c     Gruppo        lookup    Grp(Ic)                                21
046000030627     c  n21              z-add     1             Ic
046100030630     c  n21*blank        lookup    Grp(Ic)                                21
046200030718     c                   movel     Gruppo        Grp(Ic)
046300030627     c                   add       Importo       ImC(Ic)
046400030721      *
046500030721     c                   end
046600030703      *
046700030627     c                   Endsr
046800030627      *----------------------------------------------------------------
046900030627      *  Decodifica il Network
047000030627      *----------------------------------------------------------------
047100030627     c     Netw          Begsr
047200030627      *
047300030828     c                   clear                   ImpExp            1
047400030627      *
047500030627      *  x LNP controlla Estero
047600030627     C                   clear                   OG143
047700030627     c     tasLNP        chain     AzOrg01L
047800030627     c                   if        %Found(AzOrg01L)
047900030627     C                   movel     ORGDE3        OG143
048000030627     c                   end
048100030627      *
048200030627      * Attenzione: il Network estero può essere x LNP o x LNA a seconda
048300030627      *  si tratti di IMPORT o EXPORT quindi avendo prima controllato per LNP,
048400030627      *  se non fosse risultato tale allora deve controllare x LNA.
048500030627     c                   if         §ogNTW = 'FED' or
048600030627     c                              §ogNTW = 'EEX' or
048700030627     c                              §ogNTW = 'DPD'
048800030627     c                   movel     §ogNTW        ECFNTW
048900030703      *  quindi le competenze da tenere in considerazione per il confronto sono
049000030703      *  di arrivo.
049100030828     c                   movel     'I'           ImpExp
049200030721     c                   movel     'LNP'         Estero_LN         3
049300030721     c                   z-add     tasLNP        Linea_Cod         3 0
049400030627      *
049500030627     c                   else
049600030627     C                   clear                   OG143
049700030703      * prova per LNA -> se prima, x LNP il network non è un estero
049800030627     c     tasLNA        chain     AzOrg01L
049900030627     c                   if        %Found(AzOrg01L)
050000030627     C                   movel     ORGDE3        OG143
050100030627     c                   movel     §ogNTW        ECFNTW
050200030703      *  quindi le competenze da tenere in considerazione per il confronto sono
050300030703      *  di partenza.
050400030828     c                   movel     'E'           ImpExp
050500030721     c                   movel     'LNA'         Estero_LN
050600030721     c                   z-add     tasLNA        Linea_Cod
050700030627     c                   end
050800030627     c                   end
050900030627      *
051000030627     c                   Endsr
051100030627      *----------------------------------------------------------------
