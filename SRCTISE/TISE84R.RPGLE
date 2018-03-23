000100141219     /*END
000200040820     H DECEDIT('0,') DATEDIT(*dmy.)
000300050704      * ?------------------------------------------------------------------ */
000400050704      * ? Deve leggere le bolle saltando le spedizioni presenti su TITAS00F
000500050704      * ? poichè non ancora fatturate. (IGNORE del record)
000600050704      * ? Le letture saranno su 12 mesi e solo di Bolle Export legate ai Network
000700050704      * ? selezionati.
000800050704      * ? Non dovranno essere prese in considerazione le bolle STORNATE in base
000900050704      * ? al tipo Bolla oppure devono essere ignorate quelle con NOTE di CREDITO.
001000050705      *
001100050705     ?* ***  Regola del Delta
001200050705     ?* ***     (RICAVO - COMPETENZE) = GAP
001300050705     ?* ***     (GAP x 100) : RICAVO  = DELTA PERCENTUALE
001400050704      * ?
001500050510      **********************************************************************
001600050704     ftitas38C  if   E           K DISK
001700050704     F                                     IGNORE(TITAS000)
001800050705     FTita730C  iF   E           k DISK
001900050711     FECCET30C  iF   E           k DISK
002000050711     FECCED30C  iF   E           k DISK
002100050704     ftabel00f  if   E           K DISK
002200050513     fazorg01l  if   E           K DISK
002300140612     fAZCMM01L  if   E           K DISK    USROPN
002400050705     ftntam01l  if   E           K DISK
002500050513      **
002600050712     fwfcex01l  uf a E           K DISK
002700050704      * ?------------------------------------------------------------------ */
002800040820     d KPJBA         E DS
002900050511     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003000110511     D CNCR42        E DS
003100050513     D*------------------
003200050513     D* DS REPERIMENTO DATI UTENTE
003300050513     D*-------------------
003400050513     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
003500050513     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
003600050513     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
003700050513     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
003800050513     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
003900050513     D*------------------
004000050513     D* DS "TIBS10R"
004100050513     D*------------------
004200050513     D BS10DS        E DS                  EXTNAME(TIBS10DS)
004300050705     D*------------------
004400050705     D* DS TIPI BOLLA
004500050705     D*------------------
004600050705     D DSTB          E DS
004700050705     D TBL             S              2    DIM(30)
004800050705     D RBL             S              1    DIM(30)
004900050705     D TPO             S              1    DIM(30)
005000050517      *------------------
005100050517     D* FLAG SPEDIZIONI/RICAVI PER TIPO BOLLA (C/N/R/S)
005200050517     D*------------------
005300050517     D FLAB            S              1    DIM(10)                              *FLAG TIPO BOLLA
005400050517     D FSP             S              1    DIM(10)                              *FLAG SPEDIZIONI (S/
005500050517     D FRI             S              1    DIM(10)                              *FLAG RICAVI (S/N)
005600050517     D*------------------
005700050517     D* DS FLAG RICAVI/SPEDIZIONI TIPI BOLLA
005800050517     D*------------------
005900050517     D DS2W          E DS
006000050718     D*------------------
006100050718     D* DS Settore Merceologico
006200050718     D*------------------
006300050718     D DS1L          E DS
006400050718     D SME             S              5    DIM(200)                             *Cod.SMErceologico
006500050718     D DSME            S             50    DIM(200)                             *Des.SMErceologico
006600050718     D*------------------
006700050718     D* DS Nazione
006800050718     D*------------------
006900050718     D DS15          E DS
007000050718     D NAZ             S              4    DIM(300)                             *Cod.Nazione
007100050718     D DNAZ            S             25    DIM(300)                             *Des.Nazione
007200050517     D*------------------
007300050704     D DsPassaggio   e DS                  EXTNAME(Tise83ds)
007400050513     D*------------------
007500040820     D WLBDAT          DS
007600040820     D  G02DAT                 1      8  0
007700040820     D  G02INV                 9     16  0
007800040820     D  G02ERR                17     17
007900040820     D  G02TGI                18     22  0
008000050513     D*------------------
008100050513     D* ORGANIGRAMMA
008200050513     D*------------------
008300050704     D NTWdpd          S              3
008400050704     D NTWeex          S              3
008500050704     D NTWfed          S              3
008600050704     D Og143         e ds
008700050705     D*------------------
008800050705     D* Tariffe
008900050705     D*------------------
009000050705     D DSTA01        e ds
009100050704     D*------------------
009200050704     D FILN            S              3  0 DIM(500)                             *FILIALI x NTW
009300050513     D FIL             S              3  0 DIM(500)                             *FILIALI
009400050513     D DFIL            S             20    DIM(500)
009500050704     D NTW             S              3    DIM(500)                             *NETWORK
009600050513     D ARE             S              3  0 DIM(500)                             *AREE
009700050513     D DARE            S             20    DIM(500)
009800050513     D DIV             S              1    DIM(500)                             *DIVISIONI
009900050513     D DDIV            S             20    DIM(500)
010000050705     D*------------------
010100050705     D* COMMERCIALI
010200050705     D*------------------
010300140612     D***** ds01          e ds
010400141219     d c_Max_Cmm       c                   const(9000)
010500141219     D AGE             S              7  0 DIM(c_Max_Cmm)                       *CODICI AGENTE
010600141219     D RGF             S              7  0 DIM(c_Max_Cmm)                       *CODICI RAGGRUPPAMEN
010700141219     D DAGE            S             25    DIM(c_Max_Cmm)                       *DECODIFICHE AGENTI
010800050705     D*------------------
010900040820     D  data_Oggi      s                   like(G02INV) INZ(0)
011000040826     D  almeno_uno     s              1
011100050706     D  rec_x_cliente  s              1
011200050517     D  reck_OK        s              1
011300050705     D*------------------
011400050706     d tasANNOme       ds
011500050706     D  TasAAS
011600050706     D  TasMGS
011700050706      *
011800050706     d FinoA           ds
011900050706     D  AnnoFIN                       4s 0 inz(0)
012000050706     D  MeseGGfin                     4s 0 inz(0)
012100050704      * ?------------------------------------------------------------------ */
012200050704     D  savAAS         s                   like(tasAAS)
012300050704     D  savMGS         s                   like(tasMGS)
012400050704     D  savKSC         s                   like(tasKSC)
012500050704      * ?------------------------------------------------------------------ */
012600050705     d DS_Inp_Key      ds
012700050708     D  INP_ANN                            like(cexANN)
012800050708     D  INP_MES                            like(cexMES)
012900050705     D  INP_div                            like(cexDIV)
013000050705     D  INP_did                            like(cexDID)
013100050705     D  INP_are                            like(cexARE)
013200050705     D  INP_ard                            like(cexARD)
013300050705     D  INP_fil                            like(cexFIL)
013400050705     D  INP_fid                            like(cexFID)
013500050705     D  INP_age                            like(cexAGE)
013600050705     D  INP_rgf                            like(cexAGE)
013700050705     D  INP_agd                            like(cexAGD)
013800050705     D  INP_KSC                            like(cexKSC)
013900050705     D  INP_cld                            like(cexCLD)
014000050705     D  INP_clv                            like(cexCLV)
014100050708     D  INP_dps                            like(cexDAT)
014200050718     D  INP_sme                            like(cexSME)
014300050718     D  INP_dsm                            like(cexDSM)
014400050705     D  INP_KSU                            like(cexKSU)
014500050705     D  INP_dks                            like(cexDKS)
014600050705     D  INP_NTW                            like(cexNTW)
014700050705     D  INP_NOT                            like(cexNOT)
014800050705     D  INP_NAZ                            like(cexNAZ)
014900050718     D  INP_DNA                            like(cexDNA)
015000050705     D  INP_FA                             like(cexFA)
015100050707     D  INP_DIN                            like(cexDIN)
015200050707     D  INP_DFI                            like(cexDFI)
015300050707      *
015400050706     D  INP_DAT        s              8  0
015500050705     D  INP_RBL        s              1
015600050705      *
015700050705     d DS_Inp_Dati     ds
015800050705     D  INP_TSPED                          like(cexTSPED)
015900050705     D  INP_TCOLLI                         like(cexTCOLLI)
016000050705     D  INP_TVOL                           like(cexTVOL)
016100050705     D  INP_TPES                           like(cexTPES)
016200050705     D  INP_TFAT                           like(cexTFAT)
016300090923     D  INP_TCMP                           like(cetpar)
016400090923     D  TCMPs                              like(cexTCMP)
016500050707      *
016600050707     D  INP_DAN        s                   like(cexDAN)
016700050707     D  INP_DME        s                   like(cexDME)
016800050708     D  INP_AAN        s                   like(cexAAN)
016900050708     D  INP_AME        s                   like(cexAME)
017000050704      * ?------------------------------------------------------------------ */
017100050511     D* Reperimento nome PGM
017200050511     D STATUS         SDS           333
017300050511     D  DSPGM            *PROC
017400050511     D*-------------
017500040820     D digits          C                   CONST('0123456789')
017600050705      *---------------------------------------------------------------------*
017700050711     I* DEFINIZIONE DEI TIPI RECORD DEL FILE ECCET30C
017800050711     IECCET000      03
017900050711     IECCETP00      04
018000050711     I* DEFINIZIONE DEI TIPI RECORD DEL FILE ECCED30C
018100050711     IECCED000      05
018200050711     IECCEDP00      06
018300050704      * ?------------------------------------------------------------------ */
018400050513      **  Il File è organizzato x Cliente
018500050511      * ?------------------------------------------------------------------ */
018600050704     c                   clear                   almeno_uno
018700050704      **
018800050705     C     TAS_INI       setll     TITAS38C                                     AAS/MGS/KSC
018900050704     C                   READ      TITAS38C
019000050704      **
019100050704     c                   dow       not %Eof(TITAS38C)
019200050704      **
019300050704      **  Fino all'Anno/Mese
019400050706     c                   if        TasANNOMe > FINOA
019500050704     c                   leave
019600050704     c                   end
019700050704      **
019800050704     c                   z-add     tasAAS        savAAS
019900050705     c                   z-add     tasMGS        savMGS
020000050704     c                   z-add     tasKSC        savKSC
020100050705      **
020200050705      *  Pulisce DS dove preimpostare i campi di Input di scrittura/Update
020300050705      *   del record.
020400050705     c                   clear                   DS_Inp_Key
020500050706     C                   clear                   rec_x_cliente
020600050704      **
020700050704      * ? x rottura di Codice Cliente (Pulizia campi x Totale Cliente)
020800050704      * ?              /*---------------------- */
020900050704     c                   eXSr      NEW_Cliente
021000050704      * ?              /*---------------------- */
021100050704      **
021200050704      **  Legge x ANNO/MESE/CLIENTE
021300050704     c                   dow       not %Eof(TITAS38C)  and
021400050704     c                                tasAAS = savAAS  and
021500050704     c                                tasMGS = savMGS  and
021600050704     c                                tasKSC = savKSC
021700050705      * ?              /*---------------------- */
021800050705     c                   eXSr      riga_x_riga
021900050705      * ?              /*---------------------- */
022000050704      **
022100050704     C                   READ      TITAS38C
022200050704     c                   enddo
022300050704      **
022400050704      **  Totali x Cliente Anno/Mese
022500050704      * ?              /*---------------------- */
022600050704     c                   exsr      tot_Cliente
022700050704      * ?              /*---------------------- */
022800050704     c                   enddo
022900050513      ** Totali
023000050704      * ?              /*---------------------- */
023100050704     c                   exsr      tot_Fine
023200050704      * ?              /*---------------------- */
023300050705      **
023400050510     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
023500041007     c     fine          tag
023600020326     C                   SETON                                        LR
023700050705     ?****************************************************************************
023800050705      *?   Dettaglio di ogni BOLLA
023900050705     ?****************************************************************************
024000050705     c     riga_x_riga   begsr
024100050705      *
024200050705      * controllo riga se da prendere
024300050705      * ?              /*---------------------- */
024400050705     c                   eXSr      Chk_Bolla
024500050705      * ?              /*---------------------- */
024600050705      **
024700050705      ** Se il record è da prendere in considerazione
024800050705     c                   if        reck_OK = 'S'
024900050705     c                   clear                   DS_Inp_DATI
025000050706     C                   eval      rec_x_cliente = 'S'
025100050705      * ?              /*---------------------- */
025200050705     c                   eXSr      Decod_Bolla
025300050705      * ?              /*---------------------- */
025400050705     c                   eXSr      WRI_rig_det
025500050705      * ?              /*---------------------- */
025600050705     c                   move      'S'           almeno_uno
025700050705     c                   end
025800050705      *
025900050705     c                   endsr
026000050705     ?****************************************************************************
026100050705      *?   A Rottura di Codice Cliente - Operazioni a Totale Cliente Precedente
026200050705     ?****************************************************************************
026300050705     c     NEW_Cliente   begsr
026400050705      *
026500131008      * ? Decodifiche Anagrafiche
026600050705     c                   z-add     tasKSC        inp_KSC
026700050708     c                   z-add     tasAAS        inp_ANN
026800050708     c                   movel     tasMGS        inp_MES
026900131008      **
027000131008      *?   Decodifiche ANAGRAFICHE/Tabelle
027100131008      **     Come Cliente prende solo la Ragione sociale e la data prima sped. fatturata
027200131008     C                   CLEAR                   BS69DS
027300131008     C                   CLEAR                   ACODS
027400131008     C                   MOVEL     KNSIF         I69SIF
027500131008     C                   MOVEL     inp_KSC       I69KAC
027600131008     C                   CALL      'TIBS69R'
027700131008     C                   PARM                    BS69DS
027800131008     C                   PARM                    ACODS
027900131008     C                   PARM                    INDDS
028000131008     C                   PARM                    CLPDS
028100131008     C                   PARM                    CLSDS
028200131008     c* Ragione Sociale Cliente
028300131008     C     O69ERR        IFne      '1'
028400131008     c                   movel     acorag        inp_CLD
028500131008      *  Memorizza la Data Bolla più vecchia  del CLIENTE ( non UNIFICANTE)
028600131008     c                   move      clpdps        inp_DPS
028700131008     c                   movel     clpdps        anno2             2 0
028800131008     c                   if        anno2 > 40
028900131008     c                   movel     '19'          inp_DPS
029000131008     c                   else
029100131008     c                   movel     '20'          inp_DPS
029200131008     C                   End
029300131008     c                   endIF
029400131008      * Poi
029500131008     c*  prende tutto dall'unificante
029600131007     C                   CLEAR                   BS10DS
029700131008     C*---               Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
029800131008     C                   Z-ADD     WDFIN         D10DRF                         *DATA FINALE LANCIO
029900131007     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
030000131007     C                   MOVEL     'P'           D10PAF                         *CERCA PADRE
030100131007     C                   MOVE      inp_KSC       D10COD
030200131007     C                   CALL      'TIBS10R'
030300131007     C                   PARM                    BS10DS
030400131008     C                   Z-ADD     inp_KSC       inp_KSU
030500131008IF  2C     D10ERR        IFEQ      *BLANKS                                      *HA UN PADRE
030600131008     C     D10COP        ANDNE     *ZEROS
030700131008     C                   Z-ADD     D10COP        inp_KSU                        *UNIFICANTE
030800131008E   2C                   ENDIF
030900131007      **
031000131007     C                   CLEAR                   BS69DS
031100131007     C                   CLEAR                   ACODS
031200131007     C                   MOVEL     KNSIF         I69SIF
031300131007     C                   MOVEL     inp_KSU       I69KAC
031400131008     C                   MOVEL     inp_KSU       I69KCP
031500131007     C                   CALL      'TIBS69R'
031600131007     C                   PARM                    BS69DS
031700131007     C                   PARM                    ACODS
031800131007     C                   PARM                    INDDS
031900131007     C                   PARM                    CLPDS
032000131007     C                   PARM                    CLSDS
032100131008     c* Descrizione Cliente Uificante
032200131008     C                   MOVEL     'C'           inp_CLV
032300131007     C     O69ERR        IFne      '1'
032400131008     c                   movel     clpclv        inp_CLV
032500131007     c                   movel     acorag        inp_DKS
032600131008      *
032700131008     C*  Settore Merceologico
032800131008     c                   move      acoitc        inp_SME
032900131008     C                   Z-ADD     1             F
033000131008     C     inp_SME       LOOKUP    SME(F)                                 99
033100131008IF  1C     *IN99         IFEQ      *ON
033200131008     C                   movel     DSME(F)       inp_DSM
033300131008     c                   end
033400131008      **
033500131007     c                   endIF
033600131008     C*
033700131008     C* REPERISCE IL COMMERCIALE, SE NON ESISTE IMPOSTA IL CODICE AGENTE = FIL + 0000
033800131008     C                   Z-ADD     *ZEROS        inp_AGE                        *AGENTE A ZERO  =
033900131008IF  1C     O69ERR        IFEQ      '1'                                          *ERRORE
034000131008     C     clpAGE        OREQ      *ZEROS                                       *CODICE AGENTE VUOTO
034100131008     C                   MOVEL     inp_KSU       NUM3              3 0          *CODICE FILIALE +
034200131008     C                   MOVEL     NUM3          inp_AGE                        *FIL+0000
034300131008X   1C                   ELSE                                                   *NO ERRORE
034400131008     C                   Z-ADD     clpAGE        inp_AGE                        *CODICE AGENTE
034500131008E   1C                   ENDIF
034600050705     C*--------------------
034700050705     c* Descrizione Agente Commerciale
034800050705     C*
034900050705     C* REPERISCO IL RAGGRUPPAMENTO FILIALE COMMERCIALE E RELATIVA DESCRIZIONE
035000050706     C                   Z-ADD     1             C                 5 0
035100050705     C                   SETOFF                                           99
035200050705     C     inp_AGE       LOOKUP    AGE(C)                                 99
035300050705IF  1C     *IN99         ifeq      *ON
035400050705IF  2C     RGF(C)        IFEQ      *ZEROS
035500050705     C                   Z-ADD     inp_AGE       inp_RGF
035600050705     C                   MOVEL     *ALL'*'       inp_AGD                        DESCRIZIONE RAGGRUP
035700050705X   2C                   ELSE
035800050705     C                   Z-ADD     RGF(C)        inp_RGF
035900050705     C*
036000050705     C* DESCRIZIONE CODICE AGENTE = DESCRIZIONE CODICE RAGGRUPPAMENTO
036100050705     C                   Z-ADD     1             C
036200050705     C                   SETOFF                                           99
036300050705     C     inp_RGF       LOOKUP    AGE(C)                                 99
036400050705IF  3C     *IN99         IFEQ      *ON
036500050705     C                   MOVEL     DAGE(C)       inp_AGD                        DESCRIZIONE RAGGR.TO
036600050705X   3C                   ELSE
036700050705     C                   MOVEL     *ALL'*'       inp_AGD                        DESCRIZIONE RAGGR.TO
036800050705E   3C                   ENDIF
036900050705E   2C                   ENDIF
037000050705E   1C                   ENDIF
037100131008     C*--------------------
037200131008     C* REPERISCO FILIALE/AREA/DIVISIONE DEL CLIENTE dell'UNIFICANTE
037300131008     c                   movel     inp_KSU       inp_FIL
037400131008     C                   Z-ADD     1             F
037500131008     C     inp_FIL       LOOKUP    FIL(F)                                 99
037600131008IF  1C     *IN99         IFEQ      *ON
037700131008     C                   movel     dFIL(F)       inp_FID
037800131008     C                   Z-ADD     ARE(F)        inp_ARE
037900131008     C                   movel     dARE(F)       inp_ARD
038000131008     C                   MOVEL     DIV(F)        inp_DIV
038100131008     C                   movel     dDIV(F)       inp_DID
038200131008E   1C                   Else
038300131008     C                   clear                   inp_FID
038400131008     C                   clear                   inp_ARE
038500131008     C                   clear                   inp_ARD
038600131008     C                   clear                   inp_DIV
038700131008     C                   clear                   inp_DID
038800131008E   1C                   ENDIF
038900050705     C*--------------------
039000050705     c                   endsr
039100050705     ?****************************************************************************
039200050705      *?    Controlla se deve considerare il dettaglio in esame
039300050705     ?****************************************************************************
039400050705     c     Chk_Bolla     begsr
039500050705      *
039600050704     c                   eval      reck_OK = 'S'
039700050705      **
039800050705     ?** Controlla la Linea di Arrivo se è da considerare
039900050705     ?**  deve essere compresa fra le linee con il Network
040000050705     C                   Z-ADD     1             F
040100050705     C     tasLNA        LOOKUP    FILN(F)                                99
040200050705IF  1C     *IN99         ifeq      *Off
040300050705     c                   eval      reck_OK = 'N'
040400050705     c                   goto      end_check
040500050705     c                   else
040600050705      * Imposta il Network
040700050706     c                   move      NTW(F)        inp_NTW
040800050706      **
040900050706      * ? Fedex  --> Cambia il codice Network in 2 :
041000050706      * ?       Divide il Network "FED" in Buste e Merci --> "FEB" e "FEM"
041100050706     c                   if        inp_NTW  = ntwFED
041200050706     c                   z-add     tasCTR        campo2_0          2 0
041300050726     c                   if        campo2_0 >=50
041400050706     c                   move      'FEB'         inp_NTW
041500050706     c                   end
041600051014     c                   if        campo2_0 <  50
041700050706     c                   move      'FEM'         inp_NTW
041800050706     c                   end
041900050706     c                   end
042000050706     c
042100050705     c                   end
042200050704      **
042300050718     ?** Controlla come devono essere totalizzate le bolle x qtà o valore
042400050705     C                   Z-ADD     1             F
042500050705     C     tasTBL        LOOKUP    TBL(F)                                 99
042600050705IF  1C     *IN99         ifeq      *ON
042700050705      * Imposta tipo Porto
042800050705     c                   eval      inp_FA  = TPO(F)
042900050705     c                   eval      inp_RBL = RBL(F)
043000050718     c                   end
043100050718      *
043200050718     ?** Ma Se ha Note di Credito (STORNO Totale) NON deve considerare
043300050718     ?**     il valore di tassazione ma solo le quantità.
043400050718     c                   clear                   NotaCredito       1
043500050718     c                   if        tasFA2  = 'S'
043600050718     c                   move      'S'           NotaCredito
043700050718     c                   end
043800050517      **
043900050704     c     end_check     endsr
044000050705     ?****************************************************************************
044100050705      *?    Controlla se deve considerare il dettaglio in esame
044200050705     ?****************************************************************************
044300050705     c     Decod_Bolla   begsr
044400050705      *
044500050705      * ?Controlla la Tariffa se appartiene al Network
044600050705     c                   clear                   inp_NOT
044700050705     C     keyTAM        chain     TNTAM01L
044800050706     c                   if        not %Found(TNTAM01L)
044900050706     c                   move      'N'           inp_NOT
045000050706     c                   else
045100050705     c                   move      tamFLO        dsTA01
045200050705      * ? DPD
045300050705     c                   if        tamFIE ='I' and §taDPD = 'D' and
045400050705     c                             inp_NTW <> 'DPD'
045500050705     c                   move      'N'           inp_NOT
045600050705     c                   end
045700050705      * ? Fedex
045800050705     c                   if        tamFIE ='E' and §taFED = 'F' and
045900050705     c                             inp_NTW <> 'FED'
046000050705     c                   move      'N'           inp_NOT
046100050706     c                   end
046200050705      * ? EEX
046300050705     c                   if        tamFIE ='E' and §taFED = ' ' and
046400050705     c                             inp_NTW <> 'EEX'
046500050705     c                   move      'N'           inp_NOT
046600050705     c                   end
046700050705      **
046800050705     c                   end
046900050705      *
047000050705      * ? Nazione Destinatario
047100050705     c                   eval      inp_NAZ = tasNZD
047200050718     c                   clear                   inp_DNA
047300050718     C                   Z-ADD     1             G
047400050718     C     inp_NAZ       LOOKUP    NAZ(G)                                 99
047500050718IF  1C     *IN99         ifeq      *ON
047600050718     c                   movel     DNAZ(G)       inp_DNA
047700050718     c                   end
047800050707      *
047900050707      * ? Data Spedizione
048000050707     c                   move      tasANNOme     inp_DIN
048100050707     c                   move      tasANNOme     inp_DFI
048200050705      **
048300050705     C* CONTROLLO SE LA BOLLA E' DA CONSIDERARE COME SPEDIZIONE o COME RICAVO:
048400050705     C* PER IL CONTEGGIO DEL N°SPED/COLLI/PESO/VOLUME PRENDO SOLO LE
048500050705     C* BOLLE CON FLAG FSP='S'; PER IL CONTEGGIO DEI RICAVI SOLAMENTE LE BOLLE
048600050705     C* CON FLAG FRI='S';
048700050705     C                   Z-ADD     1             G
048800050705     C                   SETOFF                                           99
048900050705     C     inp_RBL       LOOKUP    FLAB(G)                                99
049000050705IF  1C     *IN99         ifeq      *ON
049100050705     C*
049200050705      * ? Bolla VALIDA come Spedizione
049300050705IF  1C     FSP(G)        IFEQ      'S'
049400050705     c                   z-add     tasNCL        Inp_TCOLLI
049500050705     c                   z-add     1             Inp_TSPED
049600050705     c                   z-add     tasVLF        Inp_TVOL
049700050705     c                   z-add     tasPKF        Inp_TPES
049800050705E   1C                   ENDIF
049900050705     C*
050000050708      * ? Bolla VALIDA come RICAVO
050100050705IF  1C     FRI(G)        IFeq      'S'
050200050718IF  1C     NotaCredito   andeq     *blank                                       <- Non Stornata totalmente
050300050705     c                   z-add     tasPOR        Inp_TFAT
050400050705     c                   add       tasVA1        Inp_TFAT
050500050705     c                   add       tasVA2        Inp_TFAT
050600050705     c                   add       tasVA3        Inp_TFAT
050700050705     c     ktas3         setll     tita730c
050800050705     c     ktas3         reade     tita730c
050900050705     c                   dow       not %Eof(tita730c)
051000050705     c                   add       ta7VAn        Inp_TFAT
051100050705     c     ktas3         reade     tita730c
051200050705     c                   enddo
051300050708     C                   endIF
051400050705      *
051500050705      * ? IMPORTO Competenze ossia  RICAVO Presunto --> per calcolo Delta
051600050705     c                   clear                   inp_TCMP
051700050711      *
051800050711      *  se più vecchie di 10 mesi non sono più presenti su ECCEV
051900050711      *    quindi occorre andare su ECCET e ECCED
052000050711      *
052100050711     c     ktas3         chain     Eccet30C
052200050711     c                   iF        %Found(Eccet30C)
052300050711     c                   add       cetPAR        inp_TCMP
052400050711     c                   add       cetPAM        inp_TCMP
052500050711     c                   add       cetPAU        inp_TCMP
052600050711     c                   add       cetPAV        inp_TCMP
052700050711     c                   add       cetARD        inp_TCMP
052800050711     c                   add       cetARM        inp_TCMP
052900050711     c                   add       cetARU        inp_TCMP
053000050711     c                   add       cetARV        inp_TCMP
053100050711     c                   add       cetTRL        inp_TCMP
053200050711     c                   add       cetPAG        inp_TCMP
053300050711     c                   add       cetSEA        inp_TCMP
053400050711     c                   add       cetSEC        inp_TCMP
053500050711     c                   add       cetRCV        inp_TCMP
053600050711     c                   add       cetMDE        inp_TCMP
053700050711     c                   add       cetTDE        inp_TCMP
053800050711     c                   add       cetMAF        inp_TCMP
053900050711     c                   add       cetTAF        inp_TCMP
054000050711     c                   add       cetSES        inp_TCMP
054100050711      *
054200050711E   1C                   End
054300050711      *
054400050711     c     ktas3         setll     Ecced30C
054500050711     c     ktas3         reade     Ecced30C
054600050711     c                   DoW       not %Eof(Ecced30C)
054700050712      ** deve escludere i transiti
054800050712     c                   eval      transiti = 'N'
054900050712     c                   exsr      Check_Trans
055000050712     c                   if        transiti = 'N'
055100050711     c                   add       cedIMP        inp_TCMP
055200050712     c                   end
055300050711     c     ktas3         reade     Ecced30C
055400050711E   1C                   End
055500050711      *
055600050711E   1C                   EndiF
055700050705     C*
055800050705     c                   endsr
055900050705     ?****************************************************************************
056000050705      *?    Scrive Riga File di Work
056100050705     ?****************************************************************************
056200050712     c     Check_Trans   begsr
056300050705      *
056400050712      *  Competenze di transito
056500050712     c                   if        CEDCMP = 11   or
056600050712     c                             CEDCMP = 12   or
056700050712     c                             CEDCMP = 53   or
056800050712     c                             CEDCMP = 54   or
056900050712     c                             CEDCMP = 55   or
057000050712     c                             CEDCMP = 204  or
057100050712     c                             CEDCMP = 205  or
057200050712     c                             CEDCMP = 206  or
057300050712     c                             CEDCMP = 207  or
057400050712     c                             CEDCMP = 208
057500050712     c                   move      'S'           transiti          1
057600050712     c                   end
057700050712     C*
057800050712     c                   endsr
057900050712     ?****************************************************************************
058000050712      *?    Scrive Riga File di Work
058100050712     ?****************************************************************************
058200050712     c     Wri_rig_det   begsr
058300050712      *
058400050705      * ? x Chiave completa
058500050712     c     Key_CEX       chain     wfcex01l
058600050705      * ? aggiorna
058700050712     c                   iF        %Found(wfcex01L)
058800050707      * ?  data Ultima
058900050707     c                   If        inp_DFI > cexDFI
059000050707     c                   move      inp_DFI       cexDFI
059100050707     c                   eNd
059200050705      * ? Aggiunge ai Valori di Periodo
059300050705     c                   exsr      add_Valori
059400050712     c                   update    WFCEX000
059500050513      *
059600050513     c                   eLSe
059700050705      * ? scrive
059800050712     c                   clear                   WFCEX000
059900050708     c                   move      inp_ANN       CEXANN
060000050708     c                   move      inp_MES       CEXMES
060100050708     c                   move      inp_DIV       CEXDIV
060200050705     c                   move      inp_DID       CEXDID
060300050705     c                   move      inp_ARE       CEXARE
060400050705     c                   move      inp_ARD       CEXARD
060500050705     c                   move      inp_FIL       CEXFIL
060600050705     c                   move      inp_FID       CEXFID
060700050705     c                   move      inp_AGE       CEXAGE
060800050705     c                   move      inp_AGD       CEXAGD
060900050705     c                   move      inp_KSC       CEXKSC
061000050705     c                   move      inp_CLD       CEXCLD
061100050705     c                   move      inp_CLV       CEXCLV
061200050708     c                   movel     inp_DPS       CEXDAT
061300050706     c                   move      inp_SME       CEXSME
061400050718     c                   move      inp_DSM       CEXDSM
061500130927      *- se non è unificato che a se stesso imposta lo stesso codice
061600130927     c                   if        inp_KSU = 0
061700130927     c                   eval      inp_KSU = inp_KSC
061800130927     c                   end
061900050708     c                   move      inp_KSU       CEXKSU
062000050705     c                   move      inp_DKS       CEXDKS
062100050705     c                   move      inp_NTW       CEXNTW
062200050705     c                   move      inp_NOT       CEXNOT
062300050705     c                   move      inp_NAZ       CEXNAZ
062400050718     c                   move      inp_DNA       CEXDNA
062500050705     c                   move      inp_FA        CEXFA
062600050707      **
062700050707      * ?  data Prima Spedizione
062800050707     c                   z-add     inp_DIN       CEXDIN
062900050707     c                   z-add     inp_DFI       CEXDFI
063000050705      **
063100050705      * ? Imposta i Valori di Periodo
063200050707     c                   move      AnnoINI       CEXDAN
063300050707     c                   move      MeseINI       CEXDME
063400050707     c                   move      se83ANN       CEXAAN
063500050707     c                   move      se83MES       CEXAME
063600050705     c                   exsr      add_Valori
063700050712     c                   write     WFCEX000
063800050705      *
063900050513     c                   eNd
064000050517      *
064100050513     c                   endsr
064200050704     ?****************************************************************************
064300050705      *?   Aggiunge i valori di totalizzazione
064400050704     ?****************************************************************************
064500050705     C     add_valori    BEGSR
064600050704      **
064700050705     c                   add       INP_TSPED     cexTSPED
064800050705     c                   add       INP_TCOLLI    cexTCOLLI
064900050705     c                   add       INP_TVOL      cexTVOL
065000050705     c                   add       INP_TPES      cexTPES
065100050705     c                   add       INP_TFAT      cexTFAT
065200090923     c                   z-add(h)  INP_TCMP      TCMPs
065300090923     c                   add       tcmps         cexTCMP
065400050705      *
065500050705     c                   endsr
065600050705     ?****************************************************************************
065700050705      *?   Totali x Cliente
065800050705     ?****************************************************************************
065900050705     C     Tot_Cliente   BEGSR
066000050705      *
066100050705     c                   endsr
066200050705     ?****************************************************************************
066300050705      *?   INIZIALIZZAZIONE
066400050705     ?****************************************************************************
066500050705     C     *inzsr        BEGSR
066600050705      **
066700040820     C     *ENTRY        PLIST
066800040820     C                   PARM                    KPJBA
066900050513     c                   movel     kpjbu         DsPassaggio
067000050705      ** Pulizia schiere
067100050705     c                   clear                   FIL
067200050705     c                   clear                   DFIL
067300050705     c                   clear                   FILN
067400050705     c                   clear                   NTW
067500050705     c                   clear                   ARE
067600050705     c                   clear                   DARE
067700050705     c                   clear                   DIV
067800050705     c                   clear                   DDIV
067900050705     c                   clear                   FLAB
068000050705     c                   clear                   FSP
068100050705     c                   clear                   FRI
068200050705     c                   clear                   TBL
068300050705     c                   clear                   RBL
068400050705     c                   clear                   TPO
068500050705     c                   clear                   AGE
068600050705     c                   clear                   RGF
068700050705     c                   clear                   DAGE
068800050511      **
068900050511     C                   Z-ADD     1             CODUT
069000050511     C                   CALL      'X§PARUT'
069100050511     C                   PARM                    UT§DSE
069200110511     C*******            MOVEL     RAGUT         RSUT             20
069300110511     C                   MOVEL     REC42         CNCR42
069400110511     C                   MOVEL     simRGS        RSUT             20
069500041103      *
069600040820      *  imposta UDATE di elaborazione
069700040820     C                   move      *date         G02DAT
069800040820     C                   MOVE      *ZEROS        G02INV
069900040820     C                   MOVE      *BLANKS       G02ERR
070000040820     C                   CALL      'XSRDA8'
070100040820     C                   PARM                    WLBDAT
070200040820     C                   Z-ADD     G02INV        data_oggi
070300050513     C                   Z-ADD     G02INV        DATCOR            8 0          *DATA (8) IN AA/M/G
070400050513     C* IMNPOSTA DATA / ORA CORRENTE
070500050712     C                   move      G02dat        day               8 0
070600050513     C                   TIME                    W0140            14 0          *ORA (6)+ DATA (8)
070700050712     C                   MOVEl     W0140         ore               6 0
070800050706      **
070900050706     c                   if        se83Dan = 0 or se83dMe = 0
071000050704      **  anno precedente
071100050704     c     se83ANN       sub       1             annoINI           4 0
071200050704      **  e 12 mesi prima
071300050704     c                   if        se83MES < 12
071400050704     c     se83MES       add       1             meseINI           2 0
071500050704     c                   else
071600050704      **  oppure gennaio dell'anno richiesto
071700050704     c                   z-add     se83ANN       annoINI
071800050704     c                   z-add     1             meseINI
071900050704     c                   end
072000050704      **
072100050706     c                   else
072200050706     c                   z-add     se83Dan       annoINI
072300050706     c                   z-add     se83DMe       meseINI
072400050706     c                   end
072500050707      **
072600050704     c                   movel     annoINI       DataINI           6 0
072700050704     c                   move      meseINI       DataINI
072800050706     c                   movel     meseINI       MESEggINI         4 0
072900050704     c                   move      '01'          MESEggINI
073000050707      **  anno finale
073100050704     c                   movel     se83ANN       DataFIN           6 0
073200050704     c                   move      se83MES       DataFIN
073300050707     c                   z-add     se83ANN       annoFIN
073400050706     c                   movel     se83MES       MESEggFIN
073500050704     c                   move      '31'          MESEggFIN
073600050707      **
073700050707     c                   z-add     se83DAN       inp_DAN
073800050707     c                   z-add     se83DME       inp_DME
073900050708     c                   z-add     se83ANN       inp_AAN
074000050708     c                   z-add     se83MES       inp_AME
074100050704      **
074200050712     c                   move      DataINI       PDadata           6 0
074300050712     c                   move      DataFIN       Padata            6 0
074400050704      **
074500131008     C* COMPONE IL GIORNO finale DELLA DATA FINALE
074600131008     C                   MOVEL     Padata        WDFIN             8 0          *DATA INIZIALE
074700131008     C                   MOVE      28            WDFIN
074800131008     C*
074900131008     C*------- GIORNO   di   F I N E   M E S E ---------------------*
075000131008     C*
075100131008     C*  Ultimo giorno del mese
075200131008     C                   MOVE      WDFIN         MMGG_4            4 0
075300131008     C                   MOVEL     MMGG_4        Mm_2              2 0
075400131008     C                   MOVEl     WDFIN         Anno_4            4 0
075500131008      * se è febbraio
075600131008     c                   If        Mm_2 = 2
075700131008      *
075800131008      *  potrebbe essere bisestile 29 gg.
075900131008     c     Anno_4        div       4             risulta           3 0
076000131008     c                   mvr                     resto             3 0
076100131008      *  Se bisestile
076200131008     c                   If        resto = 0
076300131008     C                   MOVE      '29'          WDFIN
076400131008     c                   end
076500131008      *
076600131008     c                   else
076700131008      *
076800131008      *  se invece Aprile, Giugno, Settembre, Novembre con 30 gg.
076900131008     c                   If           Mm_2 = 4 or Mm_2 = 6 or
077000131008     c                                Mm_2 = 9 or Mm_2 = 11
077100131008     C                   MOVE      '30'          WDFIN
077200131008     c                   else
077300131008     C                   MOVE      '31'          WDFIN
077400131008     c                   end
077500131008      *
077600131008     c                   endIf
077700131008     C*
077800131008     C*----------- F I N E    M E S E ---------------------------*
077900131008     C*
078000050704     c                   clear                   ntwDPD
078100050704     c                   clear                   ntwEEX
078200050704     c                   clear                   ntwFED
078300050704      **
078400050704     c                   if        %subst(se83NTW:1:1) ='S'                     <- DPD
078500050704     c                   eval      ntwDPD ='DPD'
078600050704     c                   end
078700050704     c                   if        %subst(se83NTW:2:1) ='S'                     <- EEX
078800050704     c                   eval      ntwEEX ='EEX'
078900050704     c                   end
079000050704     c                   if        %subst(se83NTW:3:1) ='S'                     <- FED
079100050704     c                   eval      ntwFED ='FED'
079200050704     c                   end
079300040823      **
079400050511     c                   seton                                        66
079500040825     c                   clear                   prima_volta       1
079600040826     c                   clear                   almeno_uno
079700050712     C                   MOVEL     DSPGM         NOMPGM           10
079800050705     C                   Z-ADD     1             tblKUT
079900050513     C*--------------------
080000050704     C* ORGANIGRAMMA Carico solo filiali x LNA da selezionare in lettura bolle
080100050704     C*              appartenenti ai Network in esame.
080200050513     C*--------------------
080300050513     C                   Z-ADD     *ZEROS        F                 3 0
080400050513     C                   Z-ADD     *ZEROS        korFIL            3 0
080500050513     C     keyORG        SETLL     AZorg01L
080600050513     C                   READ      AZorg01L                               99
080700050513DO  1C     *IN99         DOWEQ     '0'
080800050513IF  2C     orgFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
080900050513IF  3C     orgFAG        IFNE      'V'                                          *FILIALE/AGENZIA
081000050704     c                   move      orgde3        og143
081100050704
081200050513     C                   ADD       1             F
081300050513     C                   Z-ADD     orgFIL        FIL(F)
081400050513     C                   MOVEL     orgDES        DFIL(F)
081500050704     c                   MOVEL     *blank        NTW(F)
081600050704      **
081700050704     c                   if        §OGNTW <> *Blank
081800050704     c                   if        §OGNTW = ntwDPD or
081900050704     c                             §OGNTW = ntwEEX or
082000050704     c                             §OGNTW = ntwFED
082100050704     C                   Z-ADD     orgFIL        FILN(F)
082200050704     c                   MOVEL     §OGNTW        NTW(F)
082300050704E   2C                   endIF
082400050704E   2C                   endIF
082500050704      **
082600050513     C                   MOVEL     '05'          tblCOD
082700050513     C                   MOVEL     *BLANKS       tblKEY
082800050513     C                   MOVEL     orgCAR        tblKEY
082900050513     C     keyTAB        CHAIN     TABEL00F                           99
083000050513IF  4C     *IN99         IFEQ      *OFF
083100050513     C                   Z-ADD     orgCAR        ARE(F)
083200050513     C                   MOVEL     TBLUNI        DARE(F)
083300050513E   4C                   ENDIF
083400050704      **
083500050513     C                   MOVEL     '17'          tblCOD
083600050513     C                   MOVEL     *BLANKS       tblKEY
083700050513     C                   MOVEL     orgFL3        tblKEY
083800050513     C     KEYTAB        CHAIN     TABEL00F                           99
083900050513IF  4C     *IN99         IFEQ      *OFF
084000050513     C                   MOVEL     orgFL3        DIV(F)
084100050513     C                   MOVEL     TBLUNI        DDIV(F)
084200050513E   4C                   ENDIF
084300050704
084400050513E   3C                   ENDIF
084500050513E   2C                   ENDIF
084600050704
084700050513     C                   READ      AZorg01L                               99
084800050513E   1C                   ENDDO
084900050517     C*-------------------------
085000050517     C* FLAG RICAVI/SPEDIZIONI TIPI BOLLA
085100050517     C*-------------------------
085200050517     C                   MOVEL     '2W'          tblCOD
085300050517     C     KEYTA2        SETLL     TABEL00F
085400050705     C     KEYTA2        READE     TABEL00F
085500050517     C*
085600050517     C                   Z-ADD     *ZEROS        G                 4 0
085700050705DO  1C                   DOW       not %Eof(TABEL00F)
085800050517IF  2C     TBLFLG        IFEQ      *BLANKS
085900050517     C                   ADD       1             G
086000050517     C                   MOVEL     TBLKEY        FLAB(G)
086100050517     C                   MOVEL     TBLUNI        DS2W
086200050517     C                   MOVEL     §2WFSP        FSP(G)                         *FLAG SPEDIZIONI
086300050517     C                   MOVEL     §2WFRI        FRI(G)                         *FLAG RICAVI
086400050517E   2C                   ENDIF
086500050517     C*
086600050517     C     KEYTA2        READE     TABEL00F                               99
086700050517E   1C                   ENDDO
086800050718     C*-------------------------
086900050718     C* DECODIFICHE SETTORE MERCEOLOGICO
087000050718     C*-------------------------
087100050718     C                   MOVEL     '1L'          tblCOD
087200050718     C     KEYTA2        SETLL     TABEL00F
087300050718     C     KEYTA2        READE     TABEL00F
087400050718     C*
087500050718     C                   Z-ADD     *ZEROS        G                 4 0
087600050718DO  1C                   DOW       not %Eof(TABEL00F)
087700050718IF  2C     TBLFLG        IFEQ      *BLANKS
087800050718     C                   ADD       1             G
087900050718     C                   MOVEL     TBLKEY        SME(G)
088000050718     C                   MOVEL     TBLUNI        DS1L
088100050718     C                   MOVEL     §1LDES        DSME(G)                        *DESCRIZIONE
088200050718E   2C                   ENDIF
088300050718     C*
088400050718     C     KEYTA2        READE     TABEL00F                               99
088500050718E   1C                   ENDDO
088600050718     C*-------------------------
088700050718     C* DECODIFICHE NAZIONI
088800050718     C*-------------------------
088900050718     C                   MOVEL     '15'          tblCOD
089000050718     C     KEYTA2        SETLL     TABEL00F
089100050718     C     KEYTA2        READE     TABEL00F
089200050718     C*
089300050718     C                   Z-ADD     *ZEROS        G                 4 0
089400050718DO  1C                   DOW       not %Eof(TABEL00F)
089500050718IF  2C     TBLFLG        IFEQ      *BLANKS
089600050718     C                   ADD       1             G
089700050718     C                   MOVEL     TBLKEY        NAZ(G)
089800050718     C                   MOVEL     TBLUNI        DS15
089900050718     C                   MOVEL     §15DES        DNAZ(G)
090000050718E   2C                   ENDIF
090100050718     C*
090200050718     C     KEYTA2        READE     TABEL00F                               99
090300050718E   1C                   ENDDO
090400050705     C*--------------------
090500050705     C* TIPI BOLLA
090600050705     C*--------------------
090700050705     C                   MOVEL     'TB'          tblCOD
090800050705     C                   Z-ADD     *ZEROS        G
090900050705     C     keyTA2        setll     TABEL00F
091000050705     C     keyTA2        reade     TABEL00F
091100050705DO  1C                   DOW       not %Eof(TABEL00F)
091200050705IF  2C     tblFLG        IFEQ      *BLANKS
091300050705     C                   ADD       1             G
091400050705     C                   MOVEL     tblKEY        TBL(G)
091500050705     C                   MOVEL     tblUNI        DSTB
091600050705     C                   MOVEL     §TBRBL        RBL(G)
091700050705     C                   MOVEL     §TBTPO        TPO(G)
091800050705E   2C                   ENDIF
091900050705     C     keyTA2        reade     TABEL00F
092000050705E   1C                   ENDDO
092100050705     C*--------------------
092200050705     C* COMMERCIALI
092300050705     C*--------------------
092400140612     C*********          MOVEL     '01'          tblCOD
092500140612     C*********          Z-ADD     *ZEROS        G
092600140612     C***  KEYTA2        SETLL     TABEL00F
092700140612     C***  KEYTA2        READE     TABEL00F
092800140612DO  1C*********          DOW       not %Eof(TABEL00F)
092900140612IF  2C***  TBLFLG        IFEQ      *BLANKS
093000140612     C*********          ADD       1             G
093100140612     C*********          MOVEL     TBLUNI        DS01
093200140612     C*********          MOVEL     TBLKEY        AGE(G)
093300140612     C*********          MOVEL     §01RGF        RGF(G)
093400140612     C*********          MOVEL     §01AGE        DAGE(G)
093500140612E   2C*********          ENDIF
093600140612     C***  KEYTA2        READE     TABEL00F
093700140612E   1C*********          ENDDO
093800140612      *********
093900140612     C                   Z-ADD     *ZEROS        G
094000140612     c                   open      azCMM01l
094100140612     C     *loval        SETLL     azCMM01l
094200140612     C                   READ      azCMM01l
094300140612DO  1C                   DOW          not %Eof(azCMM01l)
094400140612IF  2C                   IF           cmmATB = *blank
094500140612     C                   ADD       1             G
094600140612     C                   z-add     cmmCOD        AGE(G)
094700140612     C                   z-add     cmmUNI        RGF(G)
094800140612     C                   MOVEL     cmmDES        DAGE(G)
094900140612E   2C                   ENDIF
095000140612     C                   READ      azCMM01l
095100140612E   1C                   ENDDO
095200140612     c                   close     azCMM01l
095300050705     C*--------------------
095400050513     C* CHIAVE LETTURA AZORG01L - COMPLETA
095500050513     C     keyORG        KLIST
095600050513     C                   KFLD                    korFIL                         *FILIALE
095700050513     C*
095800050513     C* CHIAVE LETTURA TABEL00F - COMPLETA
095900050513     C     keyTAB        KLIST
096000050513     C                   KFLD                    tblKUT                         *CODICE UTENTE
096100050513     C                   KFLD                    tblCOD                         *CODICE TABELLA
096200050513     C                   KFLD                    tblKEY                         *CHIAVE TABELLA
096300050517     C*
096400050517     C* CHIAVE LETTURA TABEL00F - parziale
096500050517     C     keyTA2        KLIST
096600050517     C                   KFLD                    tblKUT                         *CODICE UTENTE
096700050517     C                   KFLD                    tblCOD                         *CODICE TABELLA
096800050704     C*
096900050704     C* CHIAVE LETTURA TITAS38C - parziale
097000050705     C     TAS_INI       KLIST
097100050704     C                   KFLD                    AnnoINI
097200050704     C                   KFLD                    MeseggINI
097300050513     C*
097400050705     C* CHIAVE LETTURA TNTAM01L
097500050705     C     keyTAM        KLIST
097600050705     C                   KFLD                    tasKSC
097700050705     C                   KFLD                    tasCTR
097800050705      *
097900050705     c     ktas3         klist
098000050705     c                   kfld                    tasAas
098100050705     c                   kfld                    tasLnp
098200050705     c                   kfld                    tasNrs
098300050705     c                   kfld                    tasNsp
098400050705     c                   kfld                    tasTbl
098500050705      **
098600050705     c     key_cex       klist
098700050708     c                   kfld                    inp_ANN
098800050708     c                   kfld                    inp_MES
098900050708     c                   kfld                    inp_KSC
099000050705     c                   kfld                    inp_NTW
099100050705     c                   kfld                    inp_NAZ
099200050705     c                   kfld                    inp_FA
099300050708     c                   kfld                    inp_NOT
099400050705      *
099500050513     C                   ENDSR
099600050705     ?****************************************************************************
099700050705      *?   Totali da  File di Work
099800050705     ?****************************************************************************
099900050705     c     Tot_Fine      begsr
100000050705      * ?--
100100050705      * ?--
100200050705     c                   endsr
100300050705     ?****************************************************************************
