000100030620     ************************************************************************
000200030624      *   Genera Record di controllo Fatture Partners Estero                *
000300161298      ***********************************************************************
000400981216     H DECEDIT('0,') DATEDIT(*DMY.)
000500161298      *---------------------------------------------------------------------*
000600050907     F***Titas30C  iF   E           k DISK
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
008500050907      **** la DsTAS sostituisce la lettura dello stesso record del TITAS
008600050907      **** avendo il pgm già selezionato il record da elaborare nel FIEU78R.
008700030624     c                   movel     kpjbu         param
008800030627      *
008900030627     c     ktbe          klist
009000030627     c                   kfld                    tbeCOD
009100030627     c                   kfld                    tbeKE1
009200030627     c                   kfld                    tbeKE2
009300030718      *
009400030718     c     kEVA          klist
009500030722     c                   kfld                    evaVAR
009600030722     c                   kfld                    evaNTW
009700030722     c                   kfld                    evaFIL
009800030718      *
009900030718     c     kEVO          klist
010000030722     c                   kfld                    evoNTW
010100030722     c                   kfld                    evoFIL
010200030721     c                   kfld                    evoVOC
010300030627      *
010400030627     c     ktas1         klist
010500030627     c                   kfld                    kAas
010600030624     c                   kfld                    kLnp
010700030624     c                   kfld                    kNrs
010800030624     c                   kfld                    kNsp
010900030624      *
011000030627     c     ktas2         klist
011100030624     c                   kfld                    kAas
011200030624     c                   kfld                    kLnp
011300030624     c                   kfld                    kNrs
011400030624     c                   kfld                    kNsp
011500030624     c                   kfld                    kTbl
011600030625      *
011700030627     c     ktas3         klist
011800030625     c                   kfld                    tasAas
011900030625     c                   kfld                    tasLnp
012000030625     c                   kfld                    tasNrs
012100030625     c                   kfld                    tasNsp
012200030625     c                   kfld                    tasTbl
012300040830      *
012400040830     c     kECF          klist
012500040830     c                   kfld                    tasAas
012600040830     c                   kfld                    tasLnp
012700040830     c                   kfld                    tasNrs
012800040830     c                   kfld                    tasNsp
012900040902      *
013000040902     c     kECF1         klist
013100040902     c                   kfld                    ecfntw
013200040902     c                   kfld                    ecfrif
013300081022      *
013400081022     c     kECF4         klist
013500081022     c                   kfld                    ecfntw
013600081022     c                   kfld                    ecfrif
013700081022     c                   kfld                    ecfgrp
013800050907      ***
013900050907     c***                if        kTbl = *blank
014000050907     c***  ktas1         setll     titas30c
014100050907     c***  ktas1         reade     titas30c
014200050907     c***                else
014300050907     c***  ktas2         setll     titas30c
014400050907     c***  ktas2         reade     titas30c
014500050907     c***                end
014600050907      ***
014700050907     c***                dow       not %Eof(titas30c)
014800030625      *
014900050907      *            * ------------------------------- *
015000030627     c                   exsr      campi_ECF
015100050907      *            * ------------------------------- *
015200050907      *
015300050907      ***
015400050907     c***                if        kTbl = *blank
015500050907     c***  ktas1         reade     titas30c
015600050907     c***                else
015700050907     c***  ktas2         reade     titas30c
015800050907     c***                end
015900050907      ***
016000050907     C***                Enddo
016100050907      ***
016200050907     c***                seton                                        LR
016300050907     c                   seton                                        RT
016400030627      *----------------------------------------------------------------
016500030627      * imposta i campi per la scrittura
016600030627      *----------------------------------------------------------------
016700030627     c     campi_ECF     Begsr
016800040830      *
016900040830     c                   clear                   Gro
017000040830     c                   clear                   Grp
017100040830     c                   clear                   ImR
017200040830     c                   clear                   ImC
017300040830     c                   clear                   ImP
017400040830     c                   clear                   DfP
017500040830     c                   clear                   NfP
017600040902     **
017700040902     **  controlla che non esistessero precedenente dei records incompleti
017800040902     **   ossia non ancora fatturati da Bartolini e già addebitati dal Partner
017900040908     **   oppure non presenti nel sistema Bartolini e già addebitati dal Partner
018000040902     c                   exsr      chk_e_delete
018100040902     **
018200040902      *-------------------
018300040830     c                   clear                   tnecf000
018400040116      *
018500040116      * Decodifica il Network
018600040116     c                   exsr      NetW
018700040909      *
018800040909      *  Se si tratta di Partners Euroexpress nel NTW imposto la Linea
018900040909      *   per poter distinguere di quale Partner si tratta
019000060303     c                   clear                   savNTW            3 0
019100040909     c                   if         ecfNTW = 'EEX'
019200040909     c                   move      Linea_Cod     ecfNTW
019300060303     c                   move      Linea_Cod     savNTW            3 0
019400040909     c                   end
019500040909      *
019600040116     c                   if        KNtw <> *blank and
019700040116     c                             Kntw <> ecfNTW
019800040116     c                   goto      No_write
019900040116     c                   End
020000030627      *  imposta UDATE
020100030627     C                   move      *date         G02DAT
020200030627     C                   MOVE      *ZEROS        G02INV
020300030627     C                   MOVE      *BLANKS       G02ERR
020400030627     C                   CALL      'XSRDA8'
020500030627     C                   PARM                    WLBDAT
020600040902      *  è diventata la data consolidamento non più la data elaborazione
020700040902     C                   Z-ADD     G02INV        data_oggi
020800040902     C*********          Z-ADD     G02INV        ECFDTG
020900030627      *
021000030627     c                   movel     tasAAS        ECFAAS
021100030627     c                   movel     tasLNP        ECFLNP
021200030627     c                   movel     tasNRS        ECFNRS
021300030627     c                   movel     tasNSP        ECFNSP
021400030627     c                   movel     tasLNA        ECFLNA
021500030627     c                   movel     tasMGS        ECFMGS
021600030627     c                   movel     tasTBL        ECFTBL
021700030627     c                   movel     tasFBR        ECFFBR
021800030627     c                   movel     tasKSC        ECFKSC
021900030627     c                   movel     tasDTI        ECFDTI
022000030627     c                   movel     tasDCM        ECFDCM
022100030627     c                   movel     tasCCA        ECFCCA
022200030627     c                   movel     tasNCL        ECFNCL
022300030627     c                   movel     tasPKF        ECFPKF
022400030627     c                   movel     tasVLF        ECFVLF
022500040902     c                   z-add     tasRMN        ECFRMN
022600030703      *
022700030703      * dta e nr.fattura del Partner
022800030707     c                   movel     tasNFT        ECFNFT
022900030707     c                   movel     tasDFT        ECFDFT
023000030627      *
023100030627     c                   exsr      Rif_Partner
023200030630      *
023300040902     c                   if        savdta = 0
023400040830     c                   movel     *zeros        ECFDTA
023500040902     c                   else
023600040902     C                   Z-ADD     data_oggi     ecfdta
023700040909     C                   move      '28'          ecfdta
023800040902     c                   end
023900040902      *
024000030627      * Imposta i Ricavi
024100050216     c                   if        tastbl <> 'AS' and tastbl <> 'FS' and
024200050216     c                             tastbl <> 'AP'
024300030627     c                   exsr      Ricavi
024400050216     c                   end
024500050216      *
024600030627      * Imposta le Competenze
024700030627     c                   exsr      Competenze
024800030627      *
024900030630      * riordina i dati immagazzinati per scrivere i records
025000030630     c                   movea     grp           gro
025100030630     c                   sorta     gro
025200030630      *   deve scrivere tanti records quanti sono i raggruppamenti messi
025300030630      *    nelle schiere
025400030630     c                   do        50            cnt               3 0
025500041015      *
025600041015     c                   select
025700041015      *
025800041015     c                   when      gro(1) = *blank and cnt = 1
025900041015     c                   move      '001'         gro(1)
026000041015      *
026100041015     c                   when      gro(cnt) = *blank and cnt > 1
026200030630     c                   leave
026300041015      *
026400041015     c                   endsl
026500030627      *
026600030723     c                   movel     gro(cnt)      ECFGRP
026700030630      *
026800030630     c                   z-add     1             Ir
026900030630     c     gro(cnt)      lookup    Grp(Ir)                                21
027000030630      * Ricavi/Competenze
027100091001     c   21              z-add(h)  ImR(Ir)       ECFIMR
027200091001     c   21              z-add(h)  ImC(Ir)       ECFIMC
027300040830      *
027400091001     c   21              z-add(h)  ImP(Ir)       ECFIMP
027500041015     c   21              z-add     DfP(Ir)       ECFDFP
027600041015     c   21              movel     NfP(Ir)       ECFNFP
027700030627      *
027800081022     c                   eval      scrivere = 'SI'
027900081022      *  solo x DPD:
028000081022      *  se presente lo stesso riferimento su altra bolla non ancora CONSOLIDATA
028100081022      *  si deve attribuire Ricavo e Competenza su quella altrimenti si scrive
028200081023     c                   if        ecfNTW = 'DPD' and ecfrif <> *blank
028300081022     c     kECF4         chain     TNECF04L
028400081022     c                   if        %Found(TNECF04L) and eXfdtg=0
028500081022      *  somma ricavi e competenze
028600081022     c                   add       ecfimR        eXfimR
028700081022     c                   add       ecfimC        eXfimC
028800081022     c                   update    tnecf4
028900081022     c                   eval      scrivere = 'NO'
029000081022     c                   end
029100081022     c                   end
029200081022      *
029300081022     c                   if        scrivere = 'SI'
029400091022     c                   if        ecfNTW = 'DPD' and ecfrif <> *blank
029500091022     c                   move(p)   1             EcfNFp
029600091022     c                   end
029700040830     c                   write     tnecf000
029800081022     c                   end
029900030630      *
030000030630     c                   EndDO
030100030703     c     No_Write      tag
030200030627      *
030300030627     c                   Endsr
030400030627      *----------------------------------------------------------------
030500040902      *  Controlla e cancella records anomali precedentemente scritti
030600030627      *----------------------------------------------------------------
030700040902     c     chk_e_delete  Begsr
030800040902      *
030900040902     c                   clear                   savdta            8 0
031000030829      *
031100040902      * Per prima cosa:
031200040902      *  Controlla che non fosse già presente un record non ancora fatturato
031300040902     c     kecf          setll     tnecf02l
031400040902     c     kecf          reade     tnecf02l
031500040902      *
031600040902     c                   dow       not %eof(tnecf02l)
031700040902      *
031800040902      *  Non deve essere Consolidato
031900040902     c                   if        ecfdtg = *zero
032000040902      * se era stato scritto precedentemente
032100040902     c                   if        ecftpr ='NFT'
032200040902      * Incasella l'importo del ricavo nel gruppo di appartenenza
032300040902      *  legato alla tabella EVA.
032400040902     c                   z-add     1             Ir                3 0
032500040902     c     ecfGRP        lookup    Grp(Ir)                                21
032600040902     c  n21              z-add     1             Ir
032700040902     c  n21*blank        lookup    Grp(Ir)                                21
032800040902     c                   movel     ecfGRP        Grp(Ir)
032900040902     c                   add       ecfIMP        ImP(Ir)
033000040902     c                   movel     ecfNFP        NfP(Ir)
033100040902     c                   z-add     ecfDFP        DfP(Ir)
033200040908     c                   z-add     data_oggi     savdta
033300040902     c                   delete    tnecf000
033400040902     c                   end
033500040902     c                   end
033600040902      *
033700040902     c     kecf          reade     tnecf02l
033800040902     c                   end
033900040902      *
034000040902      * Per seconda cosa:
034100040902      *  Controlla che fosse presente un record non trovato
034200040902      *
034300040902      * Decodifica il Network
034400040902     c                   exsr      NetW
034500040902      *  Se si tratta di Partners Euroexpress nel NTW imposto la Linea
034600040902      *   per poter distinguere di quale Partner si tratta
034700040902     c                   if         ecfNTW = 'EEX'
034800040902     c                   move      Linea_Cod     ecfNTW
034900040902     c                   end
035000040909      *
035100040902     c                   exsr      Rif_Partner
035200040902      *
035300040902     c     kecf1         setll     tnecf01l
035400040902     c     kecf1         reade     tnecf01l
035500040902      *
035600040902     c                   dow       not %eof(tnecf01l)
035700040902      *
035800040902      *  Non deve essere Consolidato
035900040902     c                   if        ecfdtg = *zero
036000040902      * se era stato scritto precedentemente
036100040902     c                   if        ecftpr ='NTR'
036200040902      * Incasella l'importo del ricavo nel gruppo di appartenenza
036300040902      *  legato alla tabella EVA.
036400040902     c                   z-add     1             Ir                3 0
036500040902     c     ecfGRP        lookup    Grp(Ir)                                21
036600040902     c  n21              z-add     1             Ir
036700040902     c  n21*blank        lookup    Grp(Ir)                                21
036800040902     c                   movel     ecfGRP        Grp(Ir)
036900040902     c                   add       ecfIMP        ImP(Ir)
037000040902     c                   movel     ecfNFP        NfP(Ir)
037100040902     c                   z-add     ecfDFP        DfP(Ir)
037200040908     c                   z-add     data_oggi     savdta
037300040902     c                   delete    tnecf1
037400040902     c                   end
037500040902     c                   end
037600040902      *
037700040902     c     kecf1         reade     tnecf01l
037800040902     c                   end
037900040902      *
038000040902     c                   Endsr
038100040902      *----------------------------------------------------------------
038200040902      *  Riferimento Partner Estero solo se una Partenza
038300040902      *----------------------------------------------------------------
038400040902     c     Rif_Partner   Begsr
038500040902      *
038600030627      *  RIFERIMENTO Mittente  da tita4
038700030627     c     ktas1         setll     tita430c
038800030627     c     ktas1         reade     tita430c
038900030627      *
039000030627     c                   dow       not %Eof(tita430c)
039100030627      *
039200030829      * Se è un DPD x LNP/Network
039300030828     c                   if        §ogNTW = 'DPD'
039400030828      *
039500070619      *  x DPD deve prendere da TITA4 il record 'I'
039600070619     c                   if        ta4TRC = 'I'
039700080414     c                   eval      ECFRIF =  %subst(ta4not:1:14)
039800080414      *
039900080414      *
040000080414      * NON VA + BENE si salta la compattazione:
040100080414     c                   goto      NOcompatta
040200080414      *
040300070619     c                   if        %subst(ta4not:1:1) = '0' and
040400070619     c                             %subst(ta4not:5:2) = '99'
040500070619     c                   eval      fld11 = %subst(ta4not:2:3) +
040600070619     c                                     %subst(ta4not:7:8)
040700070619     c                   else
040800060626     c                   eval      ECFRIF =  %subst(ta4not:1:14)
040900030627     c                   end
041000080414      *
041100080414     c     NOcompatta    tag
041200080414      *
041300070619     c                   end
041400030828      *
041500030828     c                   else
041600030828      *
041700030828      * Se è un Fedex x LNP/Network
041800070927     c*****              if        ta4TRC = 'A'
041900070927     c                   if        ta4TRC = 'E'
042000060303      *
042100070927     c****               eval      ECFRIF = %subst(ta4not:16:20)
042200070927     c                   movel     ta4not        dsbl4E
042300070927     c                   movel     §b4ERP        ecfRIF
042400060303      **
042500060303      * ?Attenzione se si tratta di EUROEXPRESS Export il riferimento
042600060303      * ?  è la nostra bolla   NON DPD o FED --> ossia savNTW diverso da 0
042700060303     c                   if        savNTW <> 0
042800060303      * ? x Export il NTW deve essere uguale alla linea Arrivo
042900060303     c                   if        savNTW = ecfLNA or ecfRIF = *blank
043000060303     c                   eval      stasaas = tasaas
043100060303     c                   eval      staslnp = taslnp
043200060303     c                   eval      stasnrs = tasnrs
043300060303     c                   eval      stasnsp = tasnsp
043400060303     c                   movel     tasBolla14    ecfRIF
043500060303     c                   end
043600060303      *
043700060303     c                   end
043800060303      *
043900030828      *  se non c'è il riferimento allora imposta il rif.numerico
044000030828     c                   if        ecfRIF = *blank
044100040902     c                   movel     tasRMN        ecfRIF
044200030828     c                   end
044300060303      *
044400030828     c                   end
044500030828      *
044600030828     c                   end
044700030627      *
044800030627     c     ktas1         reade     tita430c
044900030627     c                   enddo
045000030828      *
045100030828      *  se Non trova un riferimento prende il rif_numerico
045200030828     c                   if        ecfRIF = *blank
045300060303      *
045400030828     c                   if        §ogNTW = 'DPD'
045500070619      *
045600070619     c                   select
045700070619      *
045800070619     c                   when      tasRMN > 100000000000 and
045900070619     c                             tasRMN < 999999999999
046000070619     c                   move      tasRMN        fld12            12
046100070619     c                   movel     fld12         fld11            11
046200070619     c                   movel(p)  fld11         ecfRIF
046300070619      *
046400070619     c                   when      tasRMN > 999999999999  and
046500070619     c                             tasRMN < 99999999999999
046600070619     c                   move      tasRMN        fld14            14
046700070619     c                   movel(p)  fld14         ecfRIF
046800070619      *
046900070619     c                   endSL
047000070619      *
047100030828     c                   else
047200060303      **
047300060303      * ?Imposta come riferimento la nostra Bolla
047400060303     c                   eval      stasaas = tasaas
047500060303     c                   eval      staslnp = taslnp
047600060303     c                   eval      stasnrs = tasnrs
047700060303     c                   eval      stasnsp = tasnsp
047800060303     c                   movel     tasBolla14    ecfRIF
047900060303      **
048000030828     c                   end
048100030828     c                   end
048200030828      *
048300030829      *
048400030627     c                   Endsr
048500030627      *----------------------------------------------------------------
048600030627      *  imposta i Ricavi raggruppati su schiere
048700030627      *----------------------------------------------------------------
048800030627     c     Ricavi        Begsr
048900030627      *
049000030627      * IMPORTO Ricavo
049100030627      *  Porto
049200030627     c                   if        tasPOR > 0
049300030722     c                   clear                   evaVAR
049400030722     c                   movel     '1'           evaVAR
049500160126     c                   if        tasPOR  <> 8888888
049600030627     c                   z-add     tasPOR        importo
049700160126     c                   end
049800030718     c                   exsr      Tbe_EVA
049900030627     c                   end
050000030627      *  Varie 1
050100030627     c                   if        tasVA1 > 0
050200030722     c                   clear                   evaVAR
050300030722     c                   movel     tasSV1        evaVAR
050400160126     c                   if        tasVA1  <> 8888888
050500030627     c                   z-add     tasVA1        importo
050600160126     c                   end
050700030718     c                   exsr      Tbe_EVA
050800030627     c                   end
050900030627      *  Varie 2
051000030627     c                   if        tasVA2 > 0
051100030722     c                   clear                   evaVAR
051200030722     c                   movel     tasSV2        evaVAR
051300160126     c                   if        tasVA2  <> 8888888
051400030627     c                   z-add     tasVA2        importo
051500160126     c                   end
051600030718     c                   exsr      Tbe_EVA
051700030627     c                   end
051800030627      *  Varie 3
051900030627     c                   if        tasVA3 > 0
052000030722     c                   clear                   evaVAR
052100030722     c                   movel     tasSV3        evaVAR
052200160126     c                   if        tasVA3  <> 8888888
052300030627     c                   z-add     tasVA3        importo
052400160126     c                   end
052500030718     c                   exsr      Tbe_EVA
052600030627     c                   end
052700030627      *  Altre Varie
052800030627      *
052900030627      * Controlla quindi sul file VARIE se ci sono altre varie da aggiungere
053000030627      *  e raggruppare.
053100030627     c     ktas3         setll     tita730c
053200030627     c     ktas3         reade     tita730c
053300030627     c                   dow       not %Eof(tita730c)
053400030722     c                   clear                   evaVAR
053500030722     c                   movel     ta7SVn        evaVAR
053600160126     c                   if        ta7VAn  <> 8888888
053700030627     c                   z-add     ta7VAn        importo
053800160126     c                   end
053900030718     c                   exsr      Tbe_EVA
054000030627     c     ktas3         reade     tita730c
054100030627     c                   enddo
054200030627      *
054300030627     c                   Endsr
054400030627      *----------------------------------------------------------------
054500030627      *  Decodifica il raggruppamento per le Varie
054600030627      *----------------------------------------------------------------
054700030718     c     TBE_EVA       Begsr
054800030627      *
054900030722     c                   clear                   evaNTW
055000030722     c                   clear                   evaFIL
055100030718     c                   move      '999'         Gruppo            3
055200030721      *
055300030721      * prova se ci sono delle particolarità sul Partner + LNP/A
055400030722      *  chiave completa
055500030722     c                   movel     §ogNtw        evaNTW
055600030722     c                   move      Linea_Cod     evaFIL
055700030721     c     kEVA          setll     tnEVA01l
055800030721     C                   IF        not %EQUAL
055900030721      * se ci sono dei codici particolari da prendere in considerazione
056000030627      *  al posto di quelli standard.
056100030722      *  chiave parziale
056200030722     c                   movel     §ogNtw        evaNTW
056300030722     c                   clear                   evaFIL
056400030718     c     kEVA          setll     tnEVA01l
056500030718     C                   IF        not %EQUAL
056600030722     c                   clear                   evaNTW
056700030722     c                   clear                   evaFIL
056800030722     c     kEVA          setll     tnEVA01l
056900030718     c                   end
057000030721     c                   endIF
057100030627      *
057200030718     C                   IF        %EQUAL
057300030718      *
057400030718     C                   DOU       %EOF
057500030718     C     kEVA          Reade     tnEVA01l
057600030718     C                   IF        NOT %EOF and
057700040902     C                             evaDDA <= data_oggi and
057800040902     C                             evaADA >= data_oggi
057900030718     c                   move      evaGRP        Gruppo
058000030718     c                   leave
058100030718     C                   ENDIF
058200030718     C                   ENDDO
058300030718      *
058400030718     c                   end
058500030718      *
058600030627      * Incasella l'importo del ricavo nel gruppo di appartenenza
058700030718      *  legato alla tabella EVA.
058800030627     c                   z-add     1             Ir                3 0
058900030718     c     Gruppo        lookup    Grp(Ir)                                21
059000030627     c  n21              z-add     1             Ir
059100030630     c  n21*blank        lookup    Grp(Ir)                                21
059200030718     c                   movel     Gruppo        Grp(Ir)
059300030627     c                   add       Importo       ImR(Ir)
059400030627      *
059500030627     c                   Endsr
059600030627      *----------------------------------------------------------------
059700030627      *  imposta le Competenze raggruppate su schiere
059800030627      *----------------------------------------------------------------
059900030627     c     Competenze    Begsr
060000030627      *
060100030721     c                   clear                   ImpV0
060200030721     c                   clear                   ImpVP
060300030721     c                   clear                   CodV0
060400030721     c                   clear                   CodVP
060500030721      *
060600030627      * IMPORTO Competenze
060700030718     c     ktas3         chain     Eccev30C
060800030718     c                   if        %Found(Eccev30C)
060900030627      *
061000030721     C                   Do        50            CV                3 0
061100030721      *
061200030721     C                   If        *in01 = *on
061300030721      * esce quando non ha più Voci in skiera
061400030721     C                   If        CodV0(CV) = 0
061500030721     c                   leave
061600030721     c                   end
061700030721     c                   clear                   evoVOC
061800030721     c                   movel     CodV0(CV)     evoVOC
061900030721     c                   z-add     ImpV0(CV)     importo
062000030721     c                   exsr      Tbe_EVO
062100030721     c                   end
062200030718      *
062300030721     C                   If        *in02 = *on
062400030721      * esce quando non ha più Voci in skiera
062500030721     C                   If        CodVP(CV) = 0
062600030721     c                   leave
062700030721     c                   end
062800030721     c                   clear                   evoVOC
062900030721     c                   movel     CodVP(CV)     evoVOC
063000030721     c                   z-add     ImpVP(CV)     importo
063100030721     c                   exsr      Tbe_EVO
063200030721     c                   end
063300030721      *
063400030721     c                   endDo
063500030627      *
063600030721     C                   endIF
063700030721      *
063800030627     c                   Endsr
063900030627      *----------------------------------------------------------------
064000030627      *  Decodifica il raggruppamento per le Voci
064100030627      *----------------------------------------------------------------
064200030718     c     TBE_EVO       Begsr
064300030722      *
064400030722     c                   clear                   evoNTW
064500030722     c                   clear                   evoFIL
064600030722     c                   move      '999'         Gruppo            3
064700030722      *
064800030722      * prova se ci sono delle particolarità sul Partner + LNP/A
064900030722      *  chiave completa
065000030722     c                   movel     §ogNtw        evoNTW
065100030722     c                   move      Linea_Cod     evoFIL
065200030718     c     kEVO          setll     tnEVO01l
065300030718     C                   IF        not %EQUAL
065400030722      * se ci sono dei codici particolari da prendere in considerazione
065500030722      *  al posto di quelli standard.
065600030722      *  chiave parziale
065700030723     c                   movel     §ogNtw        evoNTW
065800030723     c                   clear                   evoFIL
065900030718     c     kEVO          setll     tnEVO01l
066000030718     C                   IF        not %EQUAL
066100030723     c                   clear                   evoNTW
066200030723     c                   clear                   evoFIL
066300030722     c     kEVO          setll     tnEVO01l
066400030718     c                   end
066500030718     c                   endIF
066600030718      *
066700030718     C                   IF        %EQUAL
066800030718      *
066900030718     C                   DOU       %EOF
067000030718     C     kEVO          Reade     tnEVO01l
067100030718     C                   IF        NOT %EOF        and
067200040902     C                             evoDDA <= data_oggi and
067300040902     C                             evoADA >= data_oggi
067400030718     c                   move      evoGRP        Gruppo
067500030718     c                   leave
067600030718     C                   ENDIF
067700030718     C                   ENDDO
067800030627      *
067900030627      * Incasella l'importo del ricavo nel gruppo di appartenenza
068000030718      *  legato alla tabella EVO.
068100030627     c                   z-add     1             Ic                3 0
068200030718     c     Gruppo        lookup    Grp(Ic)                                21
068300030627     c  n21              z-add     1             Ic
068400030630     c  n21*blank        lookup    Grp(Ic)                                21
068500030718     c                   movel     Gruppo        Grp(Ic)
068600030627     c                   add       Importo       ImC(Ic)
068700030721      *
068800030721     c                   end
068900030703      *
069000030627     c                   Endsr
069100030627      *----------------------------------------------------------------
069200030627      *  Decodifica il Network
069300030627      *----------------------------------------------------------------
069400030627     c     Netw          Begsr
069500030627      *
069600030828     c                   clear                   ImpExp            1
069700030627      *
069800030627      *  x LNP controlla Estero
069900030627     C                   clear                   OG143
070000050907     c                   z-add     1             og
070100050907     c     tasLNP        lookup    Ofil(og)                               99
070200080404     c                   if        %Equal
070300050907     C                   movel     Ode3(og)      OG143
070400030627     c                   end
070500030627      *
070600030627      * Attenzione: il Network estero può essere x LNP o x LNA a seconda
070700030627      *  si tratti di IMPORT o EXPORT quindi avendo prima controllato per LNP,
070800030627      *  se non fosse risultato tale allora deve controllare x LNA.
070900030627     c                   if         §ogNTW = 'FED' or
071000030627     c                              §ogNTW = 'EEX' or
071100030627     c                              §ogNTW = 'DPD'
071200030627     c                   movel     §ogNTW        ECFNTW
071300030703      *  quindi le competenze da tenere in considerazione per il confronto sono
071400030703      *  di arrivo.
071500030828     c                   movel     'I'           ImpExp
071600030721     c                   movel     'LNP'         Estero_LN         3
071700030721     c                   z-add     tasLNP        Linea_Cod         3 0
071800030627      *
071900030627     c                   else
072000030627     C                   clear                   OG143
072100030703      * prova per LNA -> se prima, x LNP il network non è un estero
072200050907     c                   z-add     1             og
072300050907     c     tasLNA        lookup    Ofil(og)                               99
072400080404     c                   if        %Equal
072500050907     C                   movel     Ode3(og)      OG143
072600030627     c                   movel     §ogNTW        ECFNTW
072700030703      *  quindi le competenze da tenere in considerazione per il confronto sono
072800030703      *  di partenza.
072900030828     c                   movel     'E'           ImpExp
073000030721     c                   movel     'LNA'         Estero_LN
073100030721     c                   z-add     tasLNA        Linea_Cod
073200030627     c                   end
073300030627     c                   end
073400030627      *
073500030627     c                   Endsr
073600030627      *----------------------------------------------------------------
073700050907     c     *inzsr        Begsr
073800050907      *
073900050907      *  carica AZORG una volta per tutte:
074000050907     c                   clear                   oFil
074100050907     c                   clear                   oDe3
074200050907     c                   open      AzOrg01L
074300050907     c     *loval        setll     AzOrg01L
074400050907     c                   read      AzOrg01L
074500050907     c                   DOW       not %EoF(AzOrg01L)
074600050907     c                   add       1             og
074700050907     c                   move      orgfil        oFIL(og)
074800050907     c                   move      orgde3        oDE3(og)
074900050907     c                   read      AzOrg01L
075000050907     c                   enddo
075100050907     c                   close     AzOrg01L
075200050907      *
075300050907     c                   Endsr
075400050907      *----------------------------------------------------------------
