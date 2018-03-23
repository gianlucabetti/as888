000100030620     ************************************************************************
000200030624      *   Genera Record di controllo Fatture Partners Estero                *
000300161298      ***********************************************************************
000400981216     H DECEDIT('0,') DATEDIT(*DMY.)
000500161298      *---------------------------------------------------------------------*
000600171108     FFIPND01L  iF   E           k DISK
000700030624     FTita430C  iF   E           k DISK
000800030627     FTita730C  iF   E           k DISK
000900050907     FAzOrg01L  iF   E           k DISK    usropn
001000030718     FEcCev30C  iF   E           k DISK
001100030627     FTnTbe01L  iF   E           k DISK
001200030718     FTneVA01L  iF   E           k DISK
001300030718     FTneVO01L  iF   E           k DISK
001400040830     FTNECF02L  uF a E           k DISK
001500040902     FTNECF01L  uF   E           k DISK    rename(TNECF000:TNECF1)
001600081022     FTNECF04L  uF   E           k DISK    rename(TNECF000:TNECF4) prefix(EXF:3)
001700030620      *
001800081022     D Scrivere        S              2
001900091001     D Importo         S             13  5
002000030630     D GRo             S              3    DIM(50) descend
002100030630     D GRp             S              3    DIM(50)
002200091001     D IMr             S             16  5 DIM(50)
002300091001     D IMc             S             16  5 DIM(50)
002400091001     D IMp             S             16  5 DIM(50)
002500040830     D Dfp             S              8  0 DIM(50)
002600040830     D Nfp             S             15    DIM(50)
002700030718      *
002800030721      * Importo Voci spedizione (0)
002900030721    >D EcceV0        E DS                  extname(eccev00f)
003000030721     D Ptc0            S               *   INZ(%ADDR(CEVC01))
003100030721     D CodV0           S              3  0 DIM(50)
003200030721     D                                     BASED(Ptc0)
003300030721     D Pti0            S               *   INZ(%ADDR(CEVI01))
003400090910     D ImpV0           S             13  5 DIM(50)
003500030721     D                                     BASED(Pti0)
003600030721      * Importo Voci spedizione (P)
003700030721    >D EcceVP        E DS                  extname(eccevP0f) prefix(P_)
003800030721     D Ptcv            S               *   INZ(%ADDR(P_CEVC01))
003900030721     D CodVP           S              3  0 DIM(50)
004000030721     D                                     BASED(Ptcv)
004100030721     D PtiV            S               *   INZ(%ADDR(P_CEVI01))
004200090910     D ImpVP           S             13  5 DIM(50)
004300030721     D                                     BASED(PtiV)
004400030627      *
004500030624     D KPJBA         E DS
004600030625     D OG143         E DS
004700070927     D DSbl4E        E DS
004800030625      *
004900030625     D WLBDAT          DS
005000030625     D  G02DAT                 1      8  0
005100030625     D  G02INV                 9     16  0
005200030625     D  G02ERR                17     17
005300030625     D  G02TGI                18     22  0
005400040902      *
005500040902     D  data_Oggi      s                   like(G02INV) INZ(0)
005600030624      *
005700030624     D param           DS
005800030624     D   kAas                  1      4  0
005900030624     D   kLnp                  5      7  0
006000030624     D   kNrs                  8      9  0
006100030624     D   kNsp                 10     16  0
006200030624     D   kTbl                 17     18
006300030703     D   kNtw                 19     21
006400060303      *
006500060303     D TAS_Bolla       DS
006600060303     D   sTASaas               1      4  0
006700060303     D   sTASLnp               5      7  0
006800060303     D   sTASNrs               8      9  0
006900060303     D   sTASNsp              10     16  0
007000060303     D   TASBolla14            3     16  0
007100030721      *
007200050907     D DsTAS         E DS                  EXTNAME(TITAS00F)
007300050907      *
007400050907     D  Ofil           s                   like(orgfil) DIM(999)
007500050907     D  Ode3           s                   like(orgde3) DIM(999)
007600050907     d  og             s              5  0
007700030721      *---------------------------------------------------------------------*
007800030721     I* DEFINIZIONE DEI TIPI RECORD DEL FILE ECCEV30C
007900030721     IECCEV000      01
008000030721     IECCEVP00      02
008100030620      *---------------------------------------------------------------------*
008200030624     C     *entry        plist
008300030624     C                   parm                    kpjba
008400050907     C                   parm                    DsTAS
008500171108      *
008600050907      **** la DsTAS sostituisce la lettura dello stesso record del TITAS
008700050907      **** avendo il pgm già selezionato il record da elaborare nel FIEU78R.
008800030624     c                   movel     kpjbu         param
008900030627      *
009000030627     c     ktbe          klist
009100030627     c                   kfld                    tbeCOD
009200030627     c                   kfld                    tbeKE1
009300030627     c                   kfld                    tbeKE2
009400030718      *
009500030718     c     kEVA          klist
009600030722     c                   kfld                    evaVAR
009700030722     c                   kfld                    evaNTW
009800030722     c                   kfld                    evaFIL
009900030718      *
010000030718     c     kEVO          klist
010100030722     c                   kfld                    evoNTW
010200030722     c                   kfld                    evoFIL
010300030721     c                   kfld                    evoVOC
010400030627      *
010500030627     c     ktas1         klist
010600030627     c                   kfld                    kAas
010700030624     c                   kfld                    kLnp
010800030624     c                   kfld                    kNrs
010900030624     c                   kfld                    kNsp
011000030624      *
011100030627     c     ktas2         klist
011200030624     c                   kfld                    kAas
011300030624     c                   kfld                    kLnp
011400030624     c                   kfld                    kNrs
011500030624     c                   kfld                    kNsp
011600030624     c                   kfld                    kTbl
011700030625      *
011800030627     c     ktas3         klist
011900030625     c                   kfld                    tasAas
012000030625     c                   kfld                    tasLnp
012100030625     c                   kfld                    tasNrs
012200030625     c                   kfld                    tasNsp
012300030625     c                   kfld                    tasTbl
012400040830      *
012500040830     c     kECF          klist
012600040830     c                   kfld                    tasAas
012700040830     c                   kfld                    tasLnp
012800040830     c                   kfld                    tasNrs
012900040830     c                   kfld                    tasNsp
013000040902      *
013100040902     c     kECF1         klist
013200040902     c                   kfld                    ecfntw
013300040902     c                   kfld                    ecfrif
013400081022      *
013500081022     c     kECF4         klist
013600081022     c                   kfld                    ecfntw
013700081022     c                   kfld                    ecfrif
013800081022     c                   kfld                    ecfgrp
013900030625      *
014000050907      *            * ------------------------------- *
014100030627     c                   exsr      campi_ECF
014200050907      *            * ------------------------------- *
014300050907     c                   seton                                        RT
014400030627      *----------------------------------------------------------------
014500030627      * imposta i campi per la scrittura
014600030627      *----------------------------------------------------------------
014700030627     c     campi_ECF     Begsr
014800040830      *
014900040830     c                   clear                   Gro
015000040830     c                   clear                   Grp
015100040830     c                   clear                   ImR
015200040830     c                   clear                   ImC
015300040830     c                   clear                   ImP
015400040830     c                   clear                   DfP
015500040830     c                   clear                   NfP
015600040902     **
015700040902     **  controlla che non esistessero precedenente dei records incompleti
015800040902     **   ossia non ancora fatturati da Bartolini e già addebitati dal Partner
015900040908     **   oppure non presenti nel sistema Bartolini e già addebitati dal Partner
016000040902     c                   exsr      chk_e_delete
016100040902     **
016200040902      *-------------------
016300040830     c                   clear                   tnecf000
016400040116      *
016500040116      * Decodifica il Network
016600040116     c                   exsr      NetW
016700040909      *
016800040909      *  Se si tratta di Partners Euroexpress nel NTW imposto la Linea
016900040909      *   per poter distinguere di quale Partner si tratta
017000060303     c                   clear                   savNTW            3 0
017100040909     c                   if         ecfNTW = 'EEX'
017200040909     c                   move      Linea_Cod     ecfNTW
017300060303     c                   move      Linea_Cod     savNTW            3 0
017400040909     c                   end
017500040909      *
017600040116     c                   if        KNtw <> *blank and
017700040116     c                             Kntw <> ecfNTW
017800040116     c                   goto      No_write
017900040116     c                   End
018000030627      *  imposta UDATE
018100030627     C                   move      *date         G02DAT
018200030627     C                   MOVE      *ZEROS        G02INV
018300030627     C                   MOVE      *BLANKS       G02ERR
018400030627     C                   CALL      'XSRDA8'
018500030627     C                   PARM                    WLBDAT
018600040902      *  è diventata la data consolidamento non più la data elaborazione
018700040902     C                   Z-ADD     G02INV        data_oggi
018800030627      *
018900030627     c                   movel     tasAAS        ECFAAS
019000030627     c                   movel     tasLNP        ECFLNP
019100030627     c                   movel     tasNRS        ECFNRS
019200030627     c                   movel     tasNSP        ECFNSP
019300030627     c                   movel     tasLNA        ECFLNA
019400030627     c                   movel     tasMGS        ECFMGS
019500030627     c                   movel     tasTBL        ECFTBL
019600030627     c                   movel     tasFBR        ECFFBR
019700030627     c                   movel     tasKSC        ECFKSC
019800030627     c                   movel     tasDTI        ECFDTI
019900030627     c                   movel     tasDCM        ECFDCM
020000030627     c                   movel     tasCCA        ECFCCA
020100030627     c                   movel     tasNCL        ECFNCL
020200030627     c                   movel     tasPKF        ECFPKF
020300030627     c                   movel     tasVLF        ECFVLF
020400040902     c                   z-add     tasRMN        ECFRMN
020500160527      **
020600160527     c                   if        tasNZD <>*blank
020700160527     c                   movel     tasNZD        ECFNAZ
020800160527     c                   elseIf    tasNZM <>*blank
020900160527     c                   movel     tasNZM        ECFNAZ
021000160527     c                   end
021100160527      **
021200160527     c                   movel     tasCTS        ECFCTS
021300160527     c                   movel     tasFIN        ECFFIN
021400160527     c                   movel     tasFTC        ECFFTC
021500160527     c                   movel     tasTC2        ECFTC2
021600030703      *
021700030703      * dta e nr.fattura del Partner
021800030707     c                   movel     tasNFT        ECFNFT
021900030707     c                   movel     tasDFT        ECFDFT
022000030627      *
022100030627     c                   exsr      Rif_Partner
022200030630      *
022300040902     c                   if        savdta = 0
022400040830     c                   movel     *zeros        ECFDTA
022500040902     c                   else
022600040902     C                   Z-ADD     data_oggi     ecfdta
022700040909     C                   move      '28'          ecfdta
022800040902     c                   end
022900040902      *
023000030627      * Imposta i Ricavi
023100050216     c                   if        tastbl <> 'AS' and tastbl <> 'FS' and
023200050216     c                             tastbl <> 'AP'
023300030627     c                   exsr      Ricavi
023400050216     c                   end
023500050216      *
023600030627      * Imposta le Competenze
023700030627     c                   exsr      Competenze
023800030627      *
023900030630      * riordina i dati immagazzinati per scrivere i records
024000030630     c                   movea     grp           gro
024100030630     c                   sorta     gro
024200030630      *   deve scrivere tanti records quanti sono i raggruppamenti messi
024300030630      *    nelle schiere
024400030630     c                   do        50            cnt               3 0
024500041015      *
024600041015     c                   select
024700041015      *
024800041015     c                   when      gro(1) = *blank and cnt = 1
024900041015     c                   move      '001'         gro(1)
025000041015      *
025100041015     c                   when      gro(cnt) = *blank and cnt > 1
025200030630     c                   leave
025300041015      *
025400041015     c                   endsl
025500030627      *
025600030723     c                   movel     gro(cnt)      ECFGRP
025700030630      *
025800030630     c                   z-add     1             Ir
025900030630     c     gro(cnt)      lookup    Grp(Ir)                                21
026000030630      * Ricavi/Competenze
026100091001     c   21              z-add(h)  ImR(Ir)       ECFIMR
026200091001     c   21              z-add(h)  ImC(Ir)       ECFIMC
026300040830      *
026400091001     c   21              z-add(h)  ImP(Ir)       ECFIMP
026500041015     c   21              z-add     DfP(Ir)       ECFDFP
026600041015     c   21              movel     NfP(Ir)       ECFNFP
026700030627      *
026800081022     c                   eval      scrivere = 'SI'
026900081022      *  solo x DPD:
027000081022      *  se presente lo stesso riferimento su altra bolla non ancora CONSOLIDATA
027100081022      *  si deve attribuire Ricavo e Competenza su quella altrimenti si scrive
027200081023     c                   if        ecfNTW = 'DPD' and ecfrif <> *blank
027300081022     c     kECF4         chain     TNECF04L
027400081022     c                   if        %Found(TNECF04L) and eXfdtg=0
027500081022      *  somma ricavi e competenze
027600081022     c                   add       ecfimR        eXfimR
027700081022     c                   add       ecfimC        eXfimC
027800081022     c                   update    tnecf4
027900081022     c                   eval      scrivere = 'NO'
028000081022     c                   end
028100081022     c                   end
028200081022      *
028300081022     c                   if        scrivere = 'SI'
028400091022     c                   if        ecfNTW = 'DPD' and ecfrif <> *blank
028500091022     c                   move(p)   1             EcfNFp
028600091022     c                   end
028700040830     c                   write     tnecf000
028800081022     c                   end
028900030630      *
029000030630     c                   EndDO
029100030703     c     No_Write      tag
029200030627      *
029300030627     c                   Endsr
029400030627      *----------------------------------------------------------------
029500040902      *  Controlla e cancella records anomali precedentemente scritti
029600030627      *----------------------------------------------------------------
029700040902     c     chk_e_delete  Begsr
029800040902      *
029900040902     c                   clear                   savdta            8 0
030000040902      * Per prima cosa:
030100040902      *  Controlla che non fosse già presente un record non ancora fatturato
030200040902     c     kecf          setll     tnecf02l
030300040902     c     kecf          reade     tnecf02l
030400040902      *
030500040902     c                   dow       not %eof(tnecf02l)
030600040902      *
030700040902      *  Non deve essere Consolidato
030800040902     c                   if        ecfdtg = *zero
030900040902      * se era stato scritto precedentemente
031000040902     c                   if        ecftpr ='NFT'
031100040902      * Incasella l'importo del ricavo nel gruppo di appartenenza
031200040902      *  legato alla tabella EVA.
031300040902     c                   z-add     1             Ir                3 0
031400040902     c     ecfGRP        lookup    Grp(Ir)                                21
031500040902     c  n21              z-add     1             Ir
031600040902     c  n21*blank        lookup    Grp(Ir)                                21
031700040902     c                   movel     ecfGRP        Grp(Ir)
031800040902     c                   add       ecfIMP        ImP(Ir)
031900040902     c                   movel     ecfNFP        NfP(Ir)
032000040902     c                   z-add     ecfDFP        DfP(Ir)
032100040908     c                   z-add     data_oggi     savdta
032200040902     c                   delete    tnecf000
032300040902     c                   end
032400040902     c                   end
032500040902      *
032600040902     c     kecf          reade     tnecf02l
032700040902     c                   end
032800040902      *
032900040902      * Per seconda cosa:
033000040902      *  Controlla che fosse presente un record non trovato
033100040902      *
033200040902      * Decodifica il Network
033300040902     c                   exsr      NetW
033400040902      *  Se si tratta di Partners Euroexpress nel NTW imposto la Linea
033500040902      *   per poter distinguere di quale Partner si tratta
033600040902     c                   if         ecfNTW = 'EEX'
033700040902     c                   move      Linea_Cod     ecfNTW
033800040902     c                   end
033900040909      *
034000040902     c                   exsr      Rif_Partner
034100040902      *
034200040902     c     kecf1         setll     tnecf01l
034300040902     c     kecf1         reade     tnecf01l
034400040902      *
034500040902     c                   dow       not %eof(tnecf01l)
034600040902      *
034700040902      *  Non deve essere Consolidato
034800040902     c                   if        ecfdtg = *zero
034900040902      * se era stato scritto precedentemente
035000040902     c                   if        ecftpr ='NTR'
035100040902      * Incasella l'importo del ricavo nel gruppo di appartenenza
035200040902      *  legato alla tabella EVA.
035300040902     c                   z-add     1             Ir                3 0
035400040902     c     ecfGRP        lookup    Grp(Ir)                                21
035500040902     c  n21              z-add     1             Ir
035600040902     c  n21*blank        lookup    Grp(Ir)                                21
035700040902     c                   movel     ecfGRP        Grp(Ir)
035800040902     c                   add       ecfIMP        ImP(Ir)
035900040902     c                   movel     ecfNFP        NfP(Ir)
036000040902     c                   z-add     ecfDFP        DfP(Ir)
036100040908     c                   z-add     data_oggi     savdta
036200040902     c                   delete    tnecf1
036300040902     c                   end
036400040902     c                   end
036500040902      *
036600040902     c     kecf1         reade     tnecf01l
036700040902     c                   end
036800040902      *
036900040902     c                   Endsr
037000040902      *----------------------------------------------------------------
037100040902      *  Riferimento Partner Estero solo se una Partenza
037200040902      *----------------------------------------------------------------
037300040902     c     Rif_Partner   Begsr
037400040902      *
037500171108      * Se è un DPD x LNP/Network
037600171108     c                   if        §ogNTW = 'DPD'
037700171108      *
037800171108      * invece che da TITA4 dal nuovo file FIPND con le Bolle DPD
037900171108     c     ktas1         CHAIN     FIPND01L
038000171108     c                   if        %FOUND(FIPND01L)
038100171108      * per prendere il PARCEL
038200171108     c                   eval      ECFRIF =  pndIPN
038300171108     c                   end
038400171108      *
038500171108     c                   else
038600171108      *
038700171108      *  RIFERIMENTO Mittente  da tita4  SE NON E' DPD
038800030627     c     ktas1         setll     tita430c
038900030627     c     ktas1         reade     tita430c
039000030627      *
039100030627     c                   dow       not %Eof(tita430c)
039200030627      *
039300030828      * Se è un Fedex x LNP/Network
039400070927     c                   if        ta4TRC = 'E'
039500070927     c                   movel     ta4not        dsbl4E
039600070927     c                   movel     §b4ERP        ecfRIF
039700060303      **
039800060303      * ?Attenzione se si tratta di EUROEXPRESS Export il riferimento
039900060303      * ?  è la nostra bolla   NON DPD o FED --> ossia savNTW diverso da 0
040000060303     c                   if        savNTW <> 0
040100060303      * ? x Export il NTW deve essere uguale alla linea Arrivo
040200060303     c                   if        savNTW = ecfLNA or ecfRIF = *blank
040300060303     c                   eval      stasaas = tasaas
040400060303     c                   eval      staslnp = taslnp
040500060303     c                   eval      stasnrs = tasnrs
040600060303     c                   eval      stasnsp = tasnsp
040700060303     c                   movel     tasBolla14    ecfRIF
040800060303     c                   end
040900060303      *
041000060303     c                   end
041100060303      *
041200030828      *  se non c'è il riferimento allora imposta il rif.numerico
041300030828     c                   if        ecfRIF = *blank
041400040902     c                   movel     tasRMN        ecfRIF
041500030828     c                   end
041600060303      *
041700030828     c                   end
041800030828      *
041900030627     c     ktas1         reade     tita430c
042000030627     c                   enddo
042100030828      *
042200171108     c                   endIF
042300171108      *
042400030828      *  se Non trova un riferimento prende il rif_numerico
042500030828     c                   if        ecfRIF = *blank
042600060303      *
042700030828     c                   if        §ogNTW = 'DPD'
042800070619      *
042900070619     c                   select
043000070619      *
043100070619     c                   when      tasRMN > 100000000000 and
043200070619     c                             tasRMN < 999999999999
043300070619     c                   move      tasRMN        fld12            12
043400070619     c                   movel     fld12         fld11            11
043500070619     c                   movel(p)  fld11         ecfRIF
043600070619      *
043700070619     c                   when      tasRMN > 999999999999  and
043800070619     c                             tasRMN < 99999999999999
043900070619     c                   move      tasRMN        fld14            14
044000070619     c                   movel(p)  fld14         ecfRIF
044100070619      *
044200070619     c                   endSL
044300070619      *
044400030828     c                   else
044500060303      **
044600060303      * ?Imposta come riferimento la nostra Bolla
044700060303     c                   eval      stasaas = tasaas
044800060303     c                   eval      staslnp = taslnp
044900060303     c                   eval      stasnrs = tasnrs
045000060303     c                   eval      stasnsp = tasnsp
045100060303     c                   movel     tasBolla14    ecfRIF
045200060303      **
045300030828     c                   end
045400030828     c                   end
045500030828      *
045600030829      *
045700030627     c                   Endsr
045800030627      *----------------------------------------------------------------
045900030627      *  imposta i Ricavi raggruppati su schiere
046000030627      *----------------------------------------------------------------
046100030627     c     Ricavi        Begsr
046200030627      *
046300030627      * IMPORTO Ricavo
046400030627      *  Porto
046500030627     c                   if        tasPOR > 0
046600030722     c                   clear                   evaVAR
046700030722     c                   movel     '1'           evaVAR
046800160126     c                   if        tasPOR  <> 8888888
046900030627     c                   z-add     tasPOR        importo
047000160126     c                   end
047100030718     c                   exsr      Tbe_EVA
047200030627     c                   end
047300030627      *  Varie 1
047400030627     c                   if        tasVA1 > 0
047500030722     c                   clear                   evaVAR
047600030722     c                   movel     tasSV1        evaVAR
047700160126     c                   if        tasVA1  <> 8888888
047800030627     c                   z-add     tasVA1        importo
047900160126     c                   end
048000030718     c                   exsr      Tbe_EVA
048100030627     c                   end
048200030627      *  Varie 2
048300030627     c                   if        tasVA2 > 0
048400030722     c                   clear                   evaVAR
048500030722     c                   movel     tasSV2        evaVAR
048600160126     c                   if        tasVA2  <> 8888888
048700030627     c                   z-add     tasVA2        importo
048800160126     c                   end
048900030718     c                   exsr      Tbe_EVA
049000030627     c                   end
049100030627      *  Varie 3
049200030627     c                   if        tasVA3 > 0
049300030722     c                   clear                   evaVAR
049400030722     c                   movel     tasSV3        evaVAR
049500160126     c                   if        tasVA3  <> 8888888
049600030627     c                   z-add     tasVA3        importo
049700160126     c                   end
049800030718     c                   exsr      Tbe_EVA
049900030627     c                   end
050000030627      *  Altre Varie
050100030627      *
050200030627      * Controlla quindi sul file VARIE se ci sono altre varie da aggiungere
050300030627      *  e raggruppare.
050400030627     c     ktas3         setll     tita730c
050500030627     c     ktas3         reade     tita730c
050600030627     c                   dow       not %Eof(tita730c)
050700030722     c                   clear                   evaVAR
050800030722     c                   movel     ta7SVn        evaVAR
050900160126     c                   if        ta7VAn  <> 8888888
051000030627     c                   z-add     ta7VAn        importo
051100160126     c                   end
051200030718     c                   exsr      Tbe_EVA
051300030627     c     ktas3         reade     tita730c
051400030627     c                   enddo
051500030627      *
051600030627     c                   Endsr
051700030627      *----------------------------------------------------------------
051800030627      *  Decodifica il raggruppamento per le Varie
051900030627      *----------------------------------------------------------------
052000030718     c     TBE_EVA       Begsr
052100030627      *
052200030722     c                   clear                   evaNTW
052300030722     c                   clear                   evaFIL
052400030718     c                   move      '999'         Gruppo            3
052500030721      *
052600030721      * prova se ci sono delle particolarità sul Partner + LNP/A
052700030722      *  chiave completa
052800030722     c                   movel     §ogNtw        evaNTW
052900030722     c                   move      Linea_Cod     evaFIL
053000030721     c     kEVA          setll     tnEVA01l
053100030721     C                   IF        not %EQUAL
053200030721      * se ci sono dei codici particolari da prendere in considerazione
053300030627      *  al posto di quelli standard.
053400030722      *  chiave parziale
053500030722     c                   movel     §ogNtw        evaNTW
053600030722     c                   clear                   evaFIL
053700030718     c     kEVA          setll     tnEVA01l
053800030718     C                   IF        not %EQUAL
053900030722     c                   clear                   evaNTW
054000030722     c                   clear                   evaFIL
054100030722     c     kEVA          setll     tnEVA01l
054200030718     c                   end
054300030721     c                   endIF
054400030627      *
054500030718     C                   IF        %EQUAL
054600030718      *
054700030718     C                   DOU       %EOF
054800030718     C     kEVA          Reade     tnEVA01l
054900030718     C                   IF        NOT %EOF and
055000040902     C                             evaDDA <= data_oggi and
055100040902     C                             evaADA >= data_oggi
055200030718     c                   move      evaGRP        Gruppo
055300030718     c                   leave
055400030718     C                   ENDIF
055500030718     C                   ENDDO
055600030718      *
055700030718     c                   end
055800030718      *
055900030627      * Incasella l'importo del ricavo nel gruppo di appartenenza
056000030718      *  legato alla tabella EVA.
056100030627     c                   z-add     1             Ir                3 0
056200030718     c     Gruppo        lookup    Grp(Ir)                                21
056300030627     c  n21              z-add     1             Ir
056400030630     c  n21*blank        lookup    Grp(Ir)                                21
056500030718     c                   movel     Gruppo        Grp(Ir)
056600030627     c                   add       Importo       ImR(Ir)
056700030627      *
056800030627     c                   Endsr
056900030627      *----------------------------------------------------------------
057000030627      *  imposta le Competenze raggruppate su schiere
057100030627      *----------------------------------------------------------------
057200030627     c     Competenze    Begsr
057300030627      *
057400030721     c                   clear                   ImpV0
057500030721     c                   clear                   ImpVP
057600030721     c                   clear                   CodV0
057700030721     c                   clear                   CodVP
057800030721      *
057900030627      * IMPORTO Competenze
058000030718     c     ktas3         chain     Eccev30C
058100030718     c                   if        %Found(Eccev30C)
058200030627      *
058300030721     C                   Do        50            CV                3 0
058400030721      *
058500030721     C                   If        *in01 = *on
058600030721      * esce quando non ha più Voci in skiera
058700030721     C                   If        CodV0(CV) = 0
058800030721     c                   leave
058900030721     c                   end
059000030721     c                   clear                   evoVOC
059100030721     c                   movel     CodV0(CV)     evoVOC
059200030721     c                   z-add     ImpV0(CV)     importo
059300030721     c                   exsr      Tbe_EVO
059400030721     c                   end
059500030718      *
059600030721     C                   If        *in02 = *on
059700030721      * esce quando non ha più Voci in skiera
059800030721     C                   If        CodVP(CV) = 0
059900030721     c                   leave
060000030721     c                   end
060100030721     c                   clear                   evoVOC
060200030721     c                   movel     CodVP(CV)     evoVOC
060300030721     c                   z-add     ImpVP(CV)     importo
060400030721     c                   exsr      Tbe_EVO
060500030721     c                   end
060600030721      *
060700030721     c                   endDo
060800030627      *
060900030721     C                   endIF
061000030721      *
061100030627     c                   Endsr
061200030627      *----------------------------------------------------------------
061300030627      *  Decodifica il raggruppamento per le Voci
061400030627      *----------------------------------------------------------------
061500030718     c     TBE_EVO       Begsr
061600030722      *
061700030722     c                   clear                   evoNTW
061800030722     c                   clear                   evoFIL
061900030722     c                   move      '999'         Gruppo            3
062000030722      *
062100030722      * prova se ci sono delle particolarità sul Partner + LNP/A
062200030722      *  chiave completa
062300030722     c                   movel     §ogNtw        evoNTW
062400030722     c                   move      Linea_Cod     evoFIL
062500030718     c     kEVO          setll     tnEVO01l
062600030718     C                   IF        not %EQUAL
062700030722      * se ci sono dei codici particolari da prendere in considerazione
062800030722      *  al posto di quelli standard.
062900030722      *  chiave parziale
063000030723     c                   movel     §ogNtw        evoNTW
063100030723     c                   clear                   evoFIL
063200030718     c     kEVO          setll     tnEVO01l
063300030718     C                   IF        not %EQUAL
063400030723     c                   clear                   evoNTW
063500030723     c                   clear                   evoFIL
063600030722     c     kEVO          setll     tnEVO01l
063700030718     c                   end
063800030718     c                   endIF
063900030718      *
064000030718     C                   IF        %EQUAL
064100030718      *
064200030718     C                   DOU       %EOF
064300030718     C     kEVO          Reade     tnEVO01l
064400030718     C                   IF        NOT %EOF        and
064500040902     C                             evoDDA <= data_oggi and
064600040902     C                             evoADA >= data_oggi
064700030718     c                   move      evoGRP        Gruppo
064800030718     c                   leave
064900030718     C                   ENDIF
065000030718     C                   ENDDO
065100030627      *
065200030627      * Incasella l'importo del ricavo nel gruppo di appartenenza
065300030718      *  legato alla tabella EVO.
065400030627     c                   z-add     1             Ic                3 0
065500030718     c     Gruppo        lookup    Grp(Ic)                                21
065600030627     c  n21              z-add     1             Ic
065700030630     c  n21*blank        lookup    Grp(Ic)                                21
065800030718     c                   movel     Gruppo        Grp(Ic)
065900030627     c                   add       Importo       ImC(Ic)
066000030721      *
066100030721     c                   end
066200030703      *
066300030627     c                   Endsr
066400030627      *----------------------------------------------------------------
066500030627      *  Decodifica il Network
066600030627      *----------------------------------------------------------------
066700030627     c     Netw          Begsr
066800030627      *
066900030828     c                   clear                   ImpExp            1
067000030627      *
067100030627      *  x LNP controlla Estero
067200030627     C                   clear                   OG143
067300050907     c                   z-add     1             og
067400050907     c     tasLNP        lookup    Ofil(og)                               99
067500080404     c                   if        %Equal
067600050907     C                   movel     Ode3(og)      OG143
067700030627     c                   end
067800030627      *
067900030627      * Attenzione: il Network estero può essere x LNP o x LNA a seconda
068000030627      *  si tratti di IMPORT o EXPORT quindi avendo prima controllato per LNP,
068100030627      *  se non fosse risultato tale allora deve controllare x LNA.
068200030627     c                   if         §ogNTW = 'FED' or
068300030627     c                              §ogNTW = 'EEX' or
068400030627     c                              §ogNTW = 'DPD'
068500030627     c                   movel     §ogNTW        ECFNTW
068600030703      *  quindi le competenze da tenere in considerazione per il confronto sono
068700030703      *  di arrivo.
068800030828     c                   movel     'I'           ImpExp
068900030721     c                   movel     'LNP'         Estero_LN         3
069000030721     c                   z-add     tasLNP        Linea_Cod         3 0
069100030627      *
069200030627     c                   else
069300030627     C                   clear                   OG143
069400030703      * prova per LNA -> se prima, x LNP il network non è un estero
069500050907     c                   z-add     1             og
069600050907     c     tasLNA        lookup    Ofil(og)                               99
069700080404     c                   if        %Equal
069800050907     C                   movel     Ode3(og)      OG143
069900030627     c                   movel     §ogNTW        ECFNTW
070000030703      *  quindi le competenze da tenere in considerazione per il confronto sono
070100030703      *  di partenza.
070200030828     c                   movel     'E'           ImpExp
070300030721     c                   movel     'LNA'         Estero_LN
070400030721     c                   z-add     tasLNA        Linea_Cod
070500030627     c                   end
070600030627     c                   end
070700030627      *
070800030627     c                   Endsr
070900030627      *----------------------------------------------------------------
071000050907     c     *inzsr        Begsr
071100050907      *
071200050907      *  carica AZORG una volta per tutte:
071300050907     c                   clear                   oFil
071400050907     c                   clear                   oDe3
071500050907     c                   open      AzOrg01L
071600050907     c     *loval        setll     AzOrg01L
071700050907     c                   read      AzOrg01L
071800050907     c                   DOW       not %EoF(AzOrg01L)
071900050907     c                   add       1             og
072000050907     c                   move      orgfil        oFIL(og)
072100050907     c                   move      orgde3        oDE3(og)
072200050907     c                   read      AzOrg01L
072300050907     c                   enddo
072400050907     c                   close     AzOrg01L
072500050907      *
072600050907     c                   Endsr
072700050907      *----------------------------------------------------------------
